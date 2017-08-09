package flop.reading

case class SyntaxError(val message: String) extends Exception(message)
