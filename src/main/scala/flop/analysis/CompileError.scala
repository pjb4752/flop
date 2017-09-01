package flop.analysis

abstract class CompileError(message: String) extends Exception(message)

object CompileError {

  def unapply(ce: CompileError): Option[(String)] = Some(ce.getMessage)

  case class SyntaxError(message: String) extends CompileError(message)
  case class ArgumentError(message: String) extends CompileError(message)
  case class ReservedWordError(message: String) extends CompileError(message)
  case class TypeError(message: String) extends CompileError(message)
  case class UndefinedError(message: String) extends CompileError(message)
  case class ModuleError(message: String) extends CompileError(message)
  case class NameError(message: String) extends CompileError(message)

  def syntaxError(specificMessage: String, genericMessage: String) = {
    SyntaxError(specificMessage + "\n\n" + genericMessage)
  }

  def argumentError(target: String, e: (Int, Int), a: Int) = {
    val message =
      s"""Incorrect number of arguments to ${target}:
         |  expected (${e._1}..${e._2}), actual: ${a}""".stripMargin

    ArgumentError(message)
  }

  def reservedWordError(name: String) = {
    val message = s"""${name} is a reserved word
                     |  it cannot be rebound or shadowed""".stripMargin

    ReservedWordError(message)
  }

  def typeError(target: String, e: String, a: Type): TypeError = {
    val message = s"""Type mismatch in ${target}
                     |  expected ${e}, actual: ${a}""".stripMargin

    TypeError(message)
  }

  def typeError(target: String, e: Type, a: Type): TypeError = {
    typeError(target, e.toString, a)
  }

  def undefinedError(name: String) = {
    UndefinedError(s"Undefined value: ${name}")
  }

  def moduleError(message: String) = {
    ModuleError(message)
  }

  def nameError(message: String) = {
    NameError(message)
  }
}
