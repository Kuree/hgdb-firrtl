package hgdb


import firrtl.{CircuitState, Transform, _}
import firrtl.annotations.{CircuitTarget, ModuleTarget, NoTargetAnnotation}
import firrtl.ir.{Block, BundleType, Circuit, Conditionally, Connect, DefInstance, DefRegister, DefWire, Field, FileInfo, Info, Reference, SubField, SubIndex, Type, VectorType}
import firrtl.options.{RegisteredTransform, ShellOption}
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

case class StatementInfo(fn_info: String, cond: String, target: Expression)

case class ModuleInstantiation(def_name: String, inst_name: String)

class ModuleDef(val m: DefModule, val mTarget: ModuleTarget) {
  private val name = m.name
  private val ports = ListBuffer[Port]()
  private val regs = ListBuffer[DefRegister]()
  private val wires = ListBuffer[DefWire]()
  private val instances = ListBuffer[ModuleInstantiation]()
  private val condStack = new Stack()
  private var stmts = ListBuffer[StatementInfo]()

  def add_port(port: Port): DontTouchAnnotation = {
    ports += port
    new DontTouchAnnotation(mTarget.ref(port.name))
  }

  def add_reg(r: DefRegister): DontTouchAnnotation = {
    regs += r
    new DontTouchAnnotation(mTarget.ref(r.name))
  }

  def add_wire(w: DefWire): DontTouchAnnotation = {
    wires += w
    new DontTouchAnnotation(mTarget.ref(w.name))
  }

  def add_instance(def_name: String, inst_name: String): Unit = {
    instances += ModuleInstantiation(def_name, inst_name)
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
        val names = get_bundle_names(b)
        names.map(s => field.name + concat_str + s)
        names
      case _ =>
        var result = ListBuffer[String]()
        result += field.name
        result
    }
  }

  // need to fix reset stuff
  def fix_stmt_cond(): Unit = {
    val reset_map = build_reset_map()
    stmts = stmts.map(stmt => {
      val target_expr = exprToString(stmt.target)
      if (reset_map.contains(target_expr)) {
        val s = StatementInfo(stmt.fn_info, reset_map(target_expr) + " && " + stmt.cond, null)
        s
      } else {
        stmt
      }
    })
  }

  def output_var[T](sb: StringBuilder, vars: ListBuffer[T]): Unit = {
    vars.foreach(v => {
      val gen_var = get_var_names(v, ".")
      val rtl_var = get_var_names(v, "_")
      for (i <- gen_var.indices)
        println_(sb, "\"" + gen_var(i) + "\" = \"" + rtl_var(i) + "\"")
    })
  }

  // need to serialize to toml format that can be directly converted into
  def serialize(): String = {
    val sb = new StringBuilder()
    println_(sb, s"[$name]")
    // we put breakpoints as a list
    if (stmts.nonEmpty) {
      // fix the reset condition first
      fix_stmt_cond()
      println_(sb, "breakpoints = [")
      stmts.foreach(s => {
        println_(sb, "[\"" + s.fn_info + "\", \"" + s.cond + "\"],")
      })
      println_(sb, "]")
    }

    println_(sb, s"[$name.instances]")
    instances.foreach(i => println_(sb, i.inst_name + " = \"" + i.def_name + "\""))

    println_(sb, s"[$name.variables]")

    output_var(sb, ports)
    output_var(sb, regs)
    output_var(sb, wires)

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

  def add_module(m: DefModule, mTarget: ModuleTarget) {
    module_defs += new ModuleDef(m, mTarget)
  }

  def current_module(): ModuleDef = {
    module_defs.last
  }

  def serialize(): Unit = {
    if (filename.nonEmpty) {
      val writer = new PrintWriter(new File(filename))
      module_defs.foreach(m => {
        val s = m.serialize()
        writer.write(s)
      }
      )
      writer.flush()
      writer.close()
    } else {
      module_defs.foreach(m => {
        val s = m.serialize()
        println(s)
      }
      )
    }
  }
}

class AnalyzeSymbolTable(filename: String, main: String) {
  private val circuitTarget = CircuitTarget(main)
  private var dontTouches = ListBuffer[DontTouchAnnotation]()

  def execute(circuit: Circuit): Seq[DontTouchAnnotation] = {
    val table = new SymbolTable(filename)
    circuit.foreachModule(visitModule(table))

    // serialize to stdout
    table.serialize()
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
  }

  def visitStatement(table: SymbolTable)(s: Statement): Unit = {
    s.foreachStmt {
      case d: DefInstance =>
        table.current_module().add_instance(d.module, d.name)
      case Conditionally(info, pred, conseq, alt) =>
        val fn = getInfo(info)
        // that particular if statement
        if (fn.nonEmpty) {
          table.current_module().add_stmt(fn, null)
        }
        // push the condition
        table.current_module().add_pred(pred)
        visitStatement(table)(conseq)
        table.current_module().pop_pred()
        table.current_module().add_not_pred(pred)
        visitStatement(table)(alt)
        table.current_module().pop_pred()
      case c: Connect =>
        val fn = getInfo(c.info)
        if (fn.nonEmpty) {
          // add to statement
          table.current_module().add_stmt(fn, c.loc)
        }
      case b: Block =>
        b.stmts.foreach(s => visitStatement(table)(s))
      case reg: DefRegister =>
        // need to add it to the register list, which will be used to compute
        val a = table.current_module().add_reg(reg)
        dontTouches += a
      case w: DefWire =>
        val a = table.current_module().add_wire(w)
        dontTouches += a
      case _ =>
    }
  }

  def getInfo(info: Info): String = {
    info match {
      case f: FileInfo =>
        f.unescaped
      case _ =>
        ""
    }
  }
}

// options
case class HGDBPassAnnotation(filename: String) extends NoTargetAnnotation {
}

object HGDBPassAnnotation {
  def parse(t: String): HGDBPassAnnotation = {
    HGDBPassAnnotation(t)
  }
}

class AnalyzeCircuit extends Transform with DependencyAPIMigration with RegisteredTransform {
  // see https://gist.github.com/seldridge/0959d714fba6857c5f71ebc7c9044fcf
  override def prerequisites: Seq[TransformDependency] = Forms.HighForm

  def execute(state: CircuitState): CircuitState = {
    val annotations = state.annotations.collect { case a: HGDBPassAnnotation => a }
    var filename: String = ""
    if (annotations.nonEmpty) {
      filename = annotations.head.filename
    }
    val circuit = state.circuit
    val pass = new AnalyzeSymbolTable(filename, circuit.main)
    val dontTouches = pass.execute(circuit)
    val newAnnotations = state.annotations ++ dontTouches
    CircuitState(state.circuit, state.form, newAnnotations, state.renames)
  }

  val options = Seq(
    new ShellOption[String](
      longOption = "--hgdb-toml",
      toAnnotationSeq = (a: String) =>
        Seq(HGDBPassAnnotation.parse(a), RunFirrtlTransformAnnotation(new AnalyzeCircuit)),
      helpText = "HGDB Toml output file",
      shortOption = Some("hgdb"),
      helpValueName = Some("filename.toml")
    )
  )
}
