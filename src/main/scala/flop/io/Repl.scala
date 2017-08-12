package flop.io

import scala.io.StdIn
import scala.sys.process._

import flop.analysis.{Analysis, CompileError, ModuleTree}
import flop.backend.{Backend, State => EState}
import flop.reading.{Reading, SyntaxError}

object Repl {

  def repl(debug: Boolean = true): Unit =
    doRepl(ModuleTree.newRoot("user"), debug)

  @scala.annotation.tailrec
  def doRepl(tree: ModuleTree, debug: Boolean): Unit = {
    val line = StdIn.readLine("%s", "-> ")

    if (line == null || line == "(exit)") {
      println("Goodbye")
    } else if (line.isEmpty) {
      println("")
    } else {
      val newTree = eval(line, tree, debug)
      doRepl(newTree, debug)
    }
  }

  private def eval(line: String, tree: ModuleTree, debug: Boolean): ModuleTree = {
    var finalTree = tree

    try {
      val forms = Reading.read(line)
      if (debug) {
        println("--- reader forms ---")
        println(forms)
      }

      val nodes = Analysis.analyze(tree, forms)
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
      finalTree = tree
    } catch {
      case SyntaxError(m) => println(s"Syntax Error: ${m}")
      case CompileError(m) => println(s"Compile Error: ${m}")
    }
    finalTree
  }
}
