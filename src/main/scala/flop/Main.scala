package flop

import scala.io.StdIn

object Main extends App {

  @scala.annotation.tailrec
  def repl(): Unit = {
    val line = StdIn.readLine("%s", "-> ")

    if (line == null || line == "(exit)") {
      println("Goodbye")
    } else if (line.isEmpty) {
      println("")
    } else {
      eval(line)
      repl()
    }
  }

  private def eval(line: String): Unit = {
    try {
      val forms = Read.read(line)
      val nodes = Analyze.analyze(forms)
      println(forms)
      println(nodes)
    } catch {
      case Read.SyntaxError(m) => println(s"Syntax Error: ${m}")
      case Analyze.CompileError(m) => println(s"Compile Error: ${m}")
    }
  }

  repl()
}
