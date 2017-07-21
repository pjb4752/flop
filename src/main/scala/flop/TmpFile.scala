package flop

import java.io.{BufferedWriter, File, FileWriter, IOException}

object TmpFile {

  def withTmpFile(source: String, fn: File => Unit): Unit = {
    var file: File = null

    try {
      file = makeTmpFile()
      val writer = new BufferedWriter(new FileWriter(file))
      writer.write(source)
      writer.close()

      fn(file)
    } catch {
      case e: Throwable => println(e.toString)
    } finally {
      if (file != null) {
        file.delete()
      }
    }
  }

  def makeTmpFile(): File = {
    val prefix = "flop"
    val suffix = ".tmp"

    val tmpFile = File.createTempFile(prefix, suffix)
    tmpFile.deleteOnExit()
    tmpFile
  }
}
