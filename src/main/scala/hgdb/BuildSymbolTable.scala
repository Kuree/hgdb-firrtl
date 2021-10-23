package hgdb


import firrtl.{CircuitState, Transform, _}
import firrtl.annotations.{Annotation, CircuitTarget, ModuleTarget, NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.ir.{Block, BundleType, Circuit, Conditionally, Connect, DefInstance, DefNode, DefRegister, DefWire, Field, FileInfo, Info, MultiInfo, Reference, SubField, SubIndex, Type, VectorType}
import firrtl.options.{Dependency, RegisteredTransform, ShellOption}
import firrtl.stage.{Forms, RunFirrtlTransformAnnotation}
import firrtl.stage.TransformManager.TransformDependency
import firrtl.transforms.DontTouchAnnotation

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
// Firrtl IR classes
import firrtl.ir.{DefModule, Expression, Port, Statement}

// scala stack is deprecated, we will wrap ListBuffer as a stack
class Stack {
  private var elements: List[String] = Nil

  def push(x: String): Unit =
    elements = x :: elements

  def peek: String = elements.head

  def pop(): String = {
    val currentTop = peek
    elements = elements.tail
    currentTop
  }

  // and the stack value together
  def and(): String = {
    if (elements.isEmpty) "1"
    else
      elements.reduce((x, y) =>
        if (y.isEmpty) x
        else x + " && " + y
      )
  }
}

object Util {
  def getInfo(info: Info): Seq[String] = {
    info match {
      case f: FileInfo =>
        Seq(f.unescaped)
      case f: MultiInfo =>
        var r = Seq[String]()
        f.infos.foreach(i => {
          r = r ++ getInfo(i)
        })
        r
      case _ =>
        Seq()
    }
  }
}

case class StatementInfo(fn_info: String, cond: String, target: Expression) extends Ordered[StatementInfo] {
  def compare(other: StatementInfo): Int = this.fn_info compare other.fn_info
}

case class ModuleInstantiation(def_name: String, inst_name: String, fn: String)

class ModuleDef(val m: DefModule, val mTarget: ModuleTarget) {
  private val name = m.name
  private val ports = ListBuffer[Port]()
  private val regs = ListBuffer[DefRegister]()
  private val wires = ListBuffer[DefWire]()
  private val nodes = ListBuffer[DefNode]()
  private val instances = ListBuffer[ModuleInstantiation]()
  private val condStack = new Stack()
  private var stmts = mutable.SortedSet[StatementInfo]()

  def add_port(port: Port): DontTouchAnnotation = {
    ports += port
    new DontTouchAnnotation(mTarget.ref(port.name))
  }

  def anno_port(port: Port): SourceNameAnnotation = {
    SourceNameAnnotation(mTarget.ref(port.name))
  }

  def add_reg(r: DefRegister): DontTouchAnnotation = {
    regs += r
    new DontTouchAnnotation(mTarget.ref(r.name))
  }

  def anno_reg(r: DefRegister): SourceNameAnnotation = {
    SourceNameAnnotation(mTarget.ref(r.name))
  }

  def add_wire(w: DefWire): DontTouchAnnotation = {
    wires += w
    new DontTouchAnnotation(mTarget.ref(w.name))
  }

  def anno_wire(w: DefWire): SourceNameAnnotation = {
    SourceNameAnnotation(mTarget.ref(w.name))
  }

  def add_node(n: DefNode): DontTouchAnnotation = {
    if (n.name(0) != '_') {
      nodes += n
    }
    // can also be a statement
    val fn = Util.getInfo(n.info)
    fn.foreach(f => {
      stmts += StatementInfo(f, "1", n.value)
    })

    new DontTouchAnnotation(mTarget.ref(n.name))
  }

  def anno_node(n: DefNode): SourceNameAnnotation = {
    SourceNameAnnotation(mTarget.ref(n.name))
  }

  def add_instance(inst: DefInstance): Unit = {
    val fns = Util.getInfo(inst.info)
    val fn = if (fns.nonEmpty) fns.head else ""
    instances += ModuleInstantiation(inst.module, inst.name, fn)
  }

  def anno_instance(inst: DefInstance): SourceNameAnnotation = {
    SourceNameAnnotation(mTarget.ref(inst.name))
  }

  def add_pred(pred: Expression): Unit = {
    val cond = exprToString(pred)
    condStack.push(cond)
  }

  def add_not_pred(pred: Expression): Unit = {
    val cond: String = exprToString(pred)
    condStack.push("!" + cond)
  }

  def pop_pred(): Unit = {
    condStack.pop()
  }

  def add_stmt(fn: String, variable: Expression): Unit = {
    val cond = condStack.and()
    val entry = StatementInfo(fn, cond, variable)
    stmts += entry
  }

  // need to build a map to compute the reset table
  // why Firrtl/Chisel adds reset information into the high form confuses me a little bit.
  // Since it's just if statement. It would be nicer for us to compute the enable condition
  private def build_reset_map(): mutable.Map[String, String] = {
    val result = mutable.Map[String, String]()
    // for each logic defined in the module, we loop them to find their reset definition
    // we will flatten them as well
    regs.foreach(r => flatten_type_reset(r, result))
    result
  }

  private def flatten_type_reset(r: DefRegister, map: mutable.Map[String, String]): Unit = {
    val reset = "!" + exprToString(r.reset)
    r.tpe match {
      case b: BundleType =>
        b.fields.foreach(f => {
          val names = get_field_names(f, "_")
          names.foreach(n => map += (r.name + "_" + n -> reset))
        })
      case _ => map += (r.name -> reset)
    }
  }

  private def get_bundle_names(b: BundleType, concat_str: String = "_"): ListBuffer[String] = {
    var result = new ListBuffer[String]()
    b.fields.foreach(f => {
      val names = get_field_names(f, concat_str)
      result = result ++ names
    })

    result
  }

  private def get_vector_names(v: VectorType, var_name: String, concat_str: String = "_"): ListBuffer[String] = {
    val result = new ListBuffer[String]()
    for (i <- 0 until v.size) {
      val names = get_var_names_from_type(v.tpe, var_name, concat_str)
      names.foreach(n => {
        if (concat_str == ".") {
          result += n + "[" + i.toString + "]"
        } else {
          result += n + concat_str + i.toString
        }
      })
    }
    result
  }

  private def get_var_names_from_type(t: Type, var_name: String, concat_str: String): ListBuffer[String] = {
    var result = new ListBuffer[String]()
    t match {
      case b: BundleType =>
        b.fields.foreach(f => {
          val names = get_field_names(f, concat_str)
          names.foreach(n => {
            result += var_name + concat_str + n
          })
        })
      case v: VectorType =>
        result = result ++ get_vector_names(v, var_name, concat_str)
      case _ => result += var_name
    }
    result
  }

  private def get_var_names[T](v: T, concat_str: String): ListBuffer[String] = {
    // if scala supports C++ style template it would be much easier and simpler
    var result = new ListBuffer[String]()
    v match {
      case p: Port =>
        result = get_var_names_from_type(p.tpe, p.name, concat_str)
      case r: DefRegister =>
        result = get_var_names_from_type(r.tpe, r.name, concat_str)
      case w: DefWire =>
        result = get_var_names_from_type(w.tpe, w.name, concat_str)
      case _ =>
    }
    result
  }

  private def get_field_names(field: Field, concat_str: String): ListBuffer[String] = {
    field.tpe match {
      case b: BundleType =>
        val names = get_bundle_names(b, concat_str)
        names.map(s => field.name + concat_str + s)
      case v: VectorType =>
        get_vector_names(v, field.name, concat_str)
      case _ =>
        val result = ListBuffer[String]()
        result += field.name
        result
    }
  }

  // need to fix reset stuff
  def fix_stmt_cond(): Unit = {
    val reset_map = build_reset_map()
    val maps = stmts.map(stmt => {
      val target_expr = exprToString(stmt.target)
      if (reset_map.contains(target_expr)) {
        val s = StatementInfo(stmt.fn_info, reset_map(target_expr) + " && " + stmt.cond, null)
        s
      } else {
        stmt
      }
    })
    stmts = mutable.SortedSet[StatementInfo]() ++ maps
  }

  def output_var[T](sb: StringBuilder, vars: ListBuffer[T]): Unit = {
    vars.foreach(v => {
      val gen_var = get_var_names(v, ".")
      val rtl_var = get_var_names(v, "_")
      for (i <- gen_var.indices)
        println_(sb, "\"" + gen_var(i) + "\" = \"" + rtl_var(i) + "\"")
    })
  }

  def output_node(sb: StringBuilder, vars: ListBuffer[DefNode]): Unit = {
    vars.foreach(n => {
      val name = n.name
      // we assume it's already flattened since it doesn't hold any expression for the target
      // the type is always UnknownType
      val fn = Util.getInfo(n.info)
      println_(sb, "\"" + name + "\" = [\"" + name + "\", \"" + fn + "\"]")
    })
  }

  def getCurrentModule(refTargets: Seq[ReferenceTarget]): Seq[ReferenceTarget] = {
    refTargets.filter(r => {
      r.module == name
    })
  }

  def getValidRefs[A](refTargets: Seq[ReferenceTarget], list: ListBuffer[A]): ListBuffer[A] = {
    list.filter(i => {
      val r = refTargets.filter(r => {
        i match {
          case m: ModuleInstantiation =>
            r.ref == m.inst_name
          case p: Port =>
            r.ref == p.name
          case w: DefWire =>
            r.ref == w.name
          case reg: DefRegister =>
            r.ref == reg.name
          case n: DefNode =>
            r.ref == n.name
          case _ => false
        }
      })
      r.nonEmpty
    })

  }

  // need to serialize to toml format that can be directly converted into
  // ref targets is used to filter out any removed variables
  def serialize(refTargets: Seq[ReferenceTarget]): String = {
    val sb = new StringBuilder()
    println_(sb, s"[$name]")
    // we put breakpoints as a list
    if (stmts.nonEmpty) {
      // fix the reset condition first
      fix_stmt_cond()
      println_(sb, "breakpoints = [")
      stmts.foreach(s => {
        if (s.fn_info.nonEmpty) {
          // filename could be empty due to optimization
          println_(sb, "[\"" + s.fn_info + "\", \"" + s.cond + "\"],")
        }
      })
      println_(sb, "]")
    }

    val currentRefs = getCurrentModule(refTargets)

    println_(sb, s"[$name.instances]")
    val valid_instances = getValidRefs(currentRefs, instances)
    valid_instances.foreach(i => println_(sb, i.inst_name + " = \"" + i.def_name + "\""))

    println_(sb, s"[$name.variables]")

    val valid_ports = getValidRefs(currentRefs, ports)
    output_var(sb, valid_ports)

    val valid_regs = getValidRefs(currentRefs, regs)
    output_var(sb, valid_regs)

    val valid_wires = getValidRefs(currentRefs, wires)
    output_var(sb, valid_wires)
    // nodes are actually local, so we need to generate based on the breakpoint
    // ids
    val valid_nodes = getValidRefs(currentRefs, nodes)
    if (valid_nodes.nonEmpty) {
      println_(sb, s"[$name.locals]")
      output_node(sb, valid_nodes)
    }

    sb.result()
  }

  private def exprToString(pred: Expression): String = {
    pred match {
      case r: Reference => r.name
      // NOTE:
      // We made some assumptions about how IO signals are flattened. This is because FIRRTL doesn't have symbol
      // table!
      case sub: SubField => exprToString(sub.expr) + "_" + sub.name
      case sub: SubIndex => exprToString(sub.expr) + "[" + sub.value.toString + "]"
      case _ =>
        ""
    }
  }

  private def println_(sb: mutable.StringBuilder, str: String): Unit = {
    sb.append(str)
    sb.append("\n")
  }

}

class SymbolTable(filename: String) {
  private val module_defs = ListBuffer[ModuleDef]()

  def add_module(m: DefModule, mTarget: ModuleTarget): Unit = {
    module_defs += new ModuleDef(m, mTarget)
  }

  def current_module(): ModuleDef = {
    module_defs.last
  }

  def serialize(refs: Seq[ReferenceTarget]): Unit = {
    if (filename.nonEmpty) {
      val writer = new PrintWriter(new File(filename))
      module_defs.foreach(m => {
        val s = m.serialize(refs)
        writer.write(s)
      }
      )
      writer.flush()
      writer.close()
    } else {
      module_defs.foreach(m => {
        val s = m.serialize(refs)
        println(s)
      }
      )
    }
  }
}

class AnalyzeSymbolTable(filename: String, main: String) {
  private val circuitTarget = CircuitTarget(main)
  private val dontTouches = ListBuffer[DontTouchAnnotation]()
  val sourceNames: ListBuffer[SourceNameAnnotation] = ListBuffer[SourceNameAnnotation]()
  val table = new SymbolTable(filename)

  def execute(circuit: Circuit): Seq[DontTouchAnnotation] = {
    circuit.foreachModule(visitModule(table))
    dontTouches
  }

  def visitModule(table: SymbolTable)(m: DefModule): Unit = {
    // Set ledger to current module name
    val mTarget = circuitTarget.module(m.name)
    table.add_module(m, mTarget)
    m.foreachPort(visitPort(table))
    m.foreachStmt(visitStatement(table))
  }

  def visitPort(table: SymbolTable)(p: Port): Unit = {
    val a = table.current_module().add_port(p)
    dontTouches += a
    sourceNames += table.current_module().anno_port(p)
  }

  def visitStatement(table: SymbolTable)(s: Statement): Unit = {
    s.foreachStmt {
      case d: DefInstance =>
        table.current_module().add_instance(d)
        sourceNames += table.current_module().anno_instance(d)
      case Conditionally(info, pred, conseq, alt) =>
        val fn = Util.getInfo(info)
        // that particular if statement
        fn.foreach(f => {
          table.current_module().add_stmt(f, null)
        })

        // push the condition
        table.current_module().add_pred(pred)
        visitStatement(table)(conseq)
        table.current_module().pop_pred()
        table.current_module().add_not_pred(pred)
        visitStatement(table)(alt)
        table.current_module().pop_pred()
      case c: Connect =>
        val fn = Util.getInfo(c.info)
        fn.foreach(f => {
          table.current_module().add_stmt(f, c.loc)
        })
      case b: Block =>
        b.stmts.foreach(s => visitStatement(table)(s))
      case reg: DefRegister =>
        // need to add it to the register list, which will be used to compute
        val a = table.current_module().add_reg(reg)
        dontTouches += a
        sourceNames += table.current_module().anno_reg(reg)
      case w: DefWire =>
        val a = table.current_module().add_wire(w)
        dontTouches += a
        sourceNames += table.current_module().anno_wire(w)
      case n: DefNode =>
        val a = table.current_module().add_node(n)
        dontTouches += a
        sourceNames += table.current_module().anno_node(n)
      case _ =>
    }
  }
}

// options
case class HGDBPassAnnotationFilenameOption(filename: String) extends NoTargetAnnotation {
}

object HGDBPassAnnotationFilenameOption {
  def parse(t: String): HGDBPassAnnotationFilenameOption = {
    HGDBPassAnnotationFilenameOption(t)
  }
}

case class HGDBPassOptimizationOption(on: Boolean) extends NoTargetAnnotation {

}

object HGDBPassOptimizationOption {
  def parse(t: String): HGDBPassOptimizationOption = {
    HGDBPassOptimizationOption(t == "0")
  }
}

case class SourceNameAnnotation(target: ReferenceTarget)
  extends SingleTargetAnnotation[ReferenceTarget] {
  def targets = Seq(target)

  def duplicate(n: ReferenceTarget): SourceNameAnnotation = this.copy(target)
}

// wrapper annotation to the symbol table class
case class HGDBSymbolTableAnnotation(table: SymbolTable) extends NoTargetAnnotation {

}

class AnalyzeCircuit extends Transform with DependencyAPIMigration with RegisteredTransform {
  // see https://gist.github.com/seldridge/0959d714fba6857c5f71ebc7c9044fcf
  override def prerequisites: Seq[TransformDependency] = Forms.HighForm
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(Dependency(firrtl.passes.PullMuxes))

  override def invalidates(xform: Transform): Boolean = false

  def execute(state: CircuitState): CircuitState = {
    var filename: String = ""
    val filename_annotation = state.annotations.collect { case a: HGDBPassAnnotationFilenameOption => a }
    if (filename_annotation.nonEmpty) {
      filename = filename_annotation.head.filename
    }

    val debug_annotation = state.annotations.collect { case a: HGDBPassOptimizationOption => a }
    var debugMode = false
    if (debug_annotation.nonEmpty) {
      debugMode = debug_annotation.head.on
    }

    val circuit = state.circuit
    val pass = new AnalyzeSymbolTable(filename, circuit.main)
    val dontTouches = pass.execute(circuit)
    var newAnnotations = state.annotations ++ pass.sourceNames ++ Seq(HGDBSymbolTableAnnotation(pass.table))
    // only add don't touch in debug mode
    if (debugMode) {
      newAnnotations = newAnnotations ++ dontTouches
    }
    CircuitState(state.circuit, state.form, newAnnotations, state.renames)
  }

  val options = Seq(
    new ShellOption[String](
      longOption = "hgdb-toml",
      toAnnotationSeq = (a: String) =>
        Seq(HGDBPassAnnotationFilenameOption.parse(a), RunFirrtlTransformAnnotation(new AnalyzeCircuit)),
      helpText = "HGDB Toml output file",
      helpValueName = Some("filename.toml")
    ),
    new ShellOption[String](
      shortOption = Some("O"),
      helpValueName = Some("Level"),
      helpText = "Optimization level. 0 for debug build",
      toAnnotationSeq = (a: String) =>
        Seq(HGDBPassOptimizationOption.parse(a), RunFirrtlTransformAnnotation(new AnalyzeCircuit)),
      longOption = "optimization"
    )
  )
}

class CollectSourceNames {

  def execute(annotations: Seq[Annotation]): Unit = {
    val sourceNames = annotations.collect { case a: SourceNameAnnotation => a}
    val refs = sourceNames.map(a => a.target)
    // need to filter out symbols that doesn't exist any more
    // need to get the symbol table out from the annotation
    val tables = annotations.collect { case a: HGDBSymbolTableAnnotation => a }
    if (tables.nonEmpty) {
      val table = tables.head
      table.table.serialize(refs)
    }
  }
}

class CollectSourceNamesTransform extends Transform with DependencyAPIMigration with RegisteredTransform {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm ++ Seq(Dependency[AnalyzeCircuit])
  override def optionalPrerequisites: Seq[TransformDependency] = Forms.LowFormOptimized

  override def invalidates(xform: Transform): Boolean = false

  def execute(state: CircuitState): CircuitState = {
    val p = new CollectSourceNames()
    p.execute(state.annotations)
    state
  }

  val options = Seq(
    new ShellOption[String](
      longOption = "-g",
      toAnnotationSeq = (a: String) =>
        Seq(HGDBPassAnnotationFilenameOption.parse(a), RunFirrtlTransformAnnotation(new CollectSourceNamesTransform)),
      helpText = "Enable debugging symbol",
    )
  )
}
