package flop.analysis

case class CompileError(val message: String) extends Exception(message)
