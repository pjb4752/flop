package flop

import scala.io.StdIn
import scala.sys.process._

object Main extends App {

  @scala.annotation.tailrec
  def repl(state: Analyze.State): Unit = {
    val line = StdIn.readLine("%s", "-> ")

    if (line == null || line == "(exit)") {
      println("Goodbye")
    } else if (line.isEmpty) {
      println("")
    } else {
      val newState = eval(line, state)
      repl(newState)
    }
  }

  private def eval(line: String, state: Analyze.State): Analyze.State = {
    var finalState = state

    try {
      val forms = Read.read(line)
      println("--- reader forms ---")
      println(forms)

      val (newState, nodes) = Analyze.analyze(state, forms)
      println("--- ast nodes ---")
      println(nodes)

      val source = Emit.emit(nodes)
      println("--- lua source ---")
      println(source)

      TmpFile.withTmpFile(source, { tmp =>
        val cmd = "/usr/local/bin/luajit %s".format(tmp.getCanonicalPath())
        val output = Process(cmd).lineStream_!

        println("--- lua output ---")
        for (s <- output) {
          println(s)
        }
      })
      finalState = newState
    } catch {
      case Read.SyntaxError(m) => println(s"Syntax Error: ${m}")
      case Analyze.CompileError(m) => println(s"Compile Error: ${m}")
    }
    finalState
  }

  repl(Analyze.State(true, Map[String, Type](), Nil))
}
