package flop

object Main {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Nothing to compile!")
    } else {
      try {
        Compile.compileAll(args.toList)
      } catch {
        case e: Throwable => println(e)
      }
    }
  }
}
