package hgdb

import firrtl.Parser.UseInfo
import firrtl.ir.Circuit
import firrtl.options.Dependency
import firrtl.passes.Pass


object Driver {
  final def main(args: Array[String]): Unit = {
    if (args.length != 1 && args.length != 2) {
      println("Usage: input.fir [output.toml]")
      return
    }

    val src = args(0)
    var out: String = ""
    if (args.length == 2) {
      out = args(1)
    }

    object DumpSymbolTable extends Pass {
      def run(c: Circuit): Circuit = {
        val pass = new AnalyzeSymbolTable(out)
        pass.execute(c)
        c
      }
    }
    val c = firrtl.Parser.parseFile(src, UseInfo)
    val t = new firrtl.stage.TransformManager(Seq(Dependency(DumpSymbolTable)))
    t.execute(firrtl.CircuitState(c, Seq()))
  }
}
