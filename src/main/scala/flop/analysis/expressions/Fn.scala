package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Fn {

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      val message = s"""Wrong number of terms in fn form"
                       |  expected: 3, actual: ${args.length}""".stripMargin
      throw syntaxError(message)
    }

    val rType = TypeExpr.analyze(table, args(0))
    val params = analyzeParams(table, args(1))
    val symbols = params.map({ case (s, t) => (s.name.name -> t) }).toMap
    val newState = state.copy(localScopes = symbols :: state.localScopes)
    val body = newState.analyzeFn(table, newState.copy(atTopLevel = false))(args(2))

    if (body.eType != rType) {
      throw CompileError.typeError("fn return", rType, body.eType)
    }

    val fnType = Type.FreeFn(params.map(_._2), rType)
    Node.FlopFn(fnType, params, body)
  }

  private def syntaxError(specificMessage: String) = {
    val genericMessage =
      """fn expressions must be of the form:
        |  (fn RETURN-TYPE PARAMS EXPR)
        |  where:
        |    RETURN-TYPE is the specified return type of the function
        |    PARAMS is a map of the form:
        |      {name1 type1 name2 type2 namen typen}
        |    EXPR is a valid expression that returns a result""".stripMargin

    CompileError.syntaxError(specificMessage, genericMessage)
  }

  private def analyzeParams(table: SymbolTable, form: Form): Node.Params = {
    val rawParams = form match {
      case Form.MapF(raw) => raw
      case _ => {
        val message = s"""fn PARAMS is expected to be a MAP
                         |  got: ${form}""".stripMargin

        throw syntaxError(message)
      }
    }

    rawParams.map({ case (n, t) => analyzeParam(table, n, t) }).toList
  }

  private def analyzeParam(table: SymbolTable, pName: Form, pType: Form): Node.Param = {
    val rawName = pName match {
      case Form.SymF(value) => value
      case _ => {
        val message = s"""fn PARAMS name is expected to be a SYMBOL
                         |  got: ${pName}""".stripMargin

        throw syntaxError(message)
      }
    }

    if (SymbolTable.isReservedName(rawName)) {
      CompileError.reservedWordError(rawName)
    }
    val symType = TypeExpr.analyze(table, pType)
    val localName = LocalName(rawName)

    (Node.SymLit(localName, symType), symType)
  }
}
