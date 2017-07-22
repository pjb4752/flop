package flop

import scala.io.StdIn
import scala.sys.process._

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
      println("--- reader forms ---")
      println(forms)

      val nodes = Analyze.analyze(forms)
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
    } catch {
      case Read.SyntaxError(m) => println(s"Syntax Error: ${m}")
      case Analyze.CompileError(m) => println(s"Compile Error: ${m}")
    }
  }

  repl()
}
