package flop.io

import scala.io.StdIn
import scala.sys.process._

import flop.analysis.{Analysis, CompileError, State => AState}
import flop.backend.{Backend, State => EState}
import flop.reading.{Reading, SyntaxError}

object Repl {

  case class State(analysisState: AState, emitState: EState)

  case object State {

    def initial: State = {
      State(AState.initial, EState.initial)
    }
  }

  def repl(debug: Boolean = true): Unit = doRepl(State.initial, debug)

  @scala.annotation.tailrec
  def doRepl(state: State, debug: Boolean): Unit = {
    val line = StdIn.readLine("%s", "-> ")

    if (line == null || line == "(exit)") {
      println("Goodbye")
    } else if (line.isEmpty) {
      println("")
    } else {
      val newState = eval(line, state, debug)
      doRepl(newState, debug)
    }
  }

  private def eval(line: String, state: State, debug: Boolean): State = {
    var finalState = state

    try {
      val forms = Reading.read(line)
      if (debug) {
        println("--- reader forms ---")
        println(forms)
      }

      val (newAState, nodes) = Analysis.analyze(state.analysisState, forms)
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
      finalState = state.copy(analysisState = newAState)
    } catch {
      case SyntaxError(m) => println(s"Syntax Error: ${m}")
      case CompileError(m) => println(s"Compile Error: ${m}")
    }
    finalState
  }
}
