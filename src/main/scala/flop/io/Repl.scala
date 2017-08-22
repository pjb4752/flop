package flop.io

import scala.io.StdIn
import scala.sys.process._

import flop.analysis.{Analysis, CompileError, SymbolTable}
import flop.backend.{Backend, State => EState}
import flop.reading.{Reading, SyntaxError}

object Repl {

  def repl(debug: Boolean = true): Unit =
    doRepl(SymbolTable.withRoot("user"), debug)

  @scala.annotation.tailrec
  def doRepl(table: SymbolTable, debug: Boolean): Unit = {
    val line = StdIn.readLine("%s", "-> ")

    if (line == null || line == "(exit)") {
      println("Goodbye")
    } else if (line.isEmpty) {
      println("")
    } else {
      val newTable = eval(line, table, debug)
      doRepl(newTable, debug)
    }
  }

  private def eval(line: String, table: SymbolTable, debug: Boolean): SymbolTable = {
    var finalTable = table

    try {
      val forms = Reading.read(line)
      if (debug) {
        println("--- reader forms ---")
        println(forms)
      }

      val nodes = Analysis.analyze(table, forms)
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
      finalTable = table
    } catch {
      case SyntaxError(m) => println(s"Syntax Error: ${m}")
      case CompileError(se) => println(s"Compile Error: ${se.message}")
    }
    finalTable
  }
}
