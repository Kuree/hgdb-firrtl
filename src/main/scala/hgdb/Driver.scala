package hgdb

import firrtl.Parser.UseInfo
import firrtl.ir.Circuit
import firrtl.options.Dependency
import firrtl.passes.Pass


object DumpSymbolTable extends Pass {
  def run(c: Circuit): Circuit = {
    val pass = new AnalyzeSymbolTable("")
    pass.execute(c)
    c
  }
}

object Driver {
  final def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: input.fir")
      return
    }

    val src = args(0)

    val c = firrtl.Parser.parseFile(src, UseInfo)
    val t = new firrtl.stage.TransformManager(Seq(Dependency(DumpSymbolTable)))
    t.execute(firrtl.CircuitState(c, Seq()))
  }
}
