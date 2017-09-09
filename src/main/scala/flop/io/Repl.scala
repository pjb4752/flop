package flop.io

import scala.io.StdIn
import scala.sys.process._

import flop.analysis._
import flop.analysis.ModuleTree._
import flop.backend.{Backend, State => EState}
import flop.reading.{Reading, SyntaxError}

object Repl {

  def repl(debug: Boolean = true): Unit = {
    val treeName = "__repl__"

    val defaultName = Name.ModuleName(treeName, List("core"), "user")
    val defaultModule = Module.initial(defaultName)

    val blankTable = SymbolTable.withRoot(treeName)
    val initialTable = SymbolTable.addModule(blankTable, defaultModule)

    doRepl(initialTable, defaultModule, debug)
  }

  @scala.annotation.tailrec
  def doRepl(table: SymbolTable, module: Module, debug: Boolean): Unit = {

    val line = StdIn.readLine("%s", "-> ")
    if (line == null || line == "(exit)") {
      println("Goodbye")
    } else if (line.isEmpty) {
      println("")
    } else {
      val newTable = eval(line, table, module, debug)
      doRepl(newTable, module, debug)
    }
  }

  private def eval(line: String, table: SymbolTable, module: Module,
      debug: Boolean): SymbolTable = {

    var finalTable = table

    try {
      val forms = Reading.read(line)
      if (debug) {
        println("--- reader forms ---")
        println(forms)
      }

      val (newTable, nodes) = Analysis.analyze(table, module, forms)
      if (debug) {
        println("--- ast nodes ---")
        println(nodes)
      }

      val source = Backend.emit(nodes)
      if (debug) {
        println("--- lua source ---")
        println(source)
      }

      TmpFile.withTmpFile(source, { tmp =>
        val cmd = "/usr/local/bin/luajit %s".format(tmp.getCanonicalPath())
        val output = Process(cmd).lineStream_!

        if (debug) {
          println("--- lua output ---")
        }

        for (s <- output) {
          println(s)
        }
      })
      finalTable = newTable
    } catch {
      case SyntaxError(m) => println(s"Syntax Error: ${m}")
      case CompileError(m) => println(s"Compile Error: ${m}")
    }
    finalTable
  }
}
