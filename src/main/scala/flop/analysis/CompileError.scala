package flop.analysis

trait Error {
  val message: String
}

case class CompileError(error: Error) extends Exception(error.message)
