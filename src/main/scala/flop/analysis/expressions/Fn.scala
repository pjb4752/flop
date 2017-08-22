package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Fn {

  type FlopError = flop.analysis.Error

  case object SyntaxError extends FlopError {
    val message = """fn expressions must be of the form:
                    |  (fn RETURN-TYPE PARAM EXPR)""".stripMargin
  }

  case class ReturnTypeError(formal: Type, actual: Type) extends FlopError {
    val message = s"""fn return types do not match:
                     |  expected: ${formal}, actual: ${actual}""".stripMargin
  }

  case object ParamSyntaxError extends FlopError {
    val message = """fn PARAM expressions must be of the form:
                    |  {NAME TYPE}""".stripMargin
  }

  case object BadNameError extends FlopError {
    val message = "fn PARAM must be a name"
  }

  case class RedefienError(name: String) extends FlopError {
    val message = s"def error: cannot redefine var ${name}"
  }

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError(SyntaxError)
    }

    val rType = SymbolTable.analyzeTypeForm(tree, args(0))
    val params = analyzeParams(tree, args(1))
    val symbols = params.map({ case (s, t) => (s.name.name -> t) }).toMap
    val newState = state.copy(localScopes = symbols :: state.localScopes)
    println(newState)
    val body = newState.analyzeFn(tree, newState.copy(atTopLevel = false))(args(2))

    if (body.eType != rType) {
      throw CompileError(ReturnTypeError(body.eType, rType))
    }

    val fnType = Type.FreeFn(params.map(_._2), rType)
    Node.FlopFn(fnType, params, body)
  }

  private def analyzeParams(tree: ModuleTree, form: Form): Node.Params = {
    val rawParams = form match {
      case Form.MapF(raw) => raw
      case _ => throw CompileError(ParamSyntaxError)
    }

    rawParams.map({ case (n, t) => analyzeParam(tree, n, t) }).toList
  }

  private def analyzeParam(tree: ModuleTree, pName: Form, pType: Form): Node.Param = {
    val rawName = pName match {
      case Form.SymF(value) => value
      case _ => throw CompileError(BadNameError)
    }

    if (SymbolTable.isReservedName(rawName)) {
      throw CompileError(RedefienError(rawName))
    }
    val symType = SymbolTable.analyzeTypeForm(tree, pType)
    val localName = LocalName(rawName)

    (Node.SymLit(localName, symType), symType)
  }
}
