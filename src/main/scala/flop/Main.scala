package flop

import scala.io.StdIn

object Main extends App {

  @scala.annotation.tailrec
  def repl(): Unit = {
    val line = StdIn.readLine("%s", "-> ")

    if (line == null || line == "(quit)") {
      println("Goodbye")
    } else {
      val forms = Read.read(line.toList)
      println(forms)
      repl()
    }
  }

  repl()
}
