package flop

object Main {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Nothing to compile!")
    } else {
      Compile.compileAll(args.toList)
    }
  }
}
