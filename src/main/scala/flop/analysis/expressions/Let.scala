package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Let {

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      val message = s"""Wrong number of terms in let form"
                       |  expected: 3, actual: ${args.length}""".stripMargin
      throw syntaxError(message)
    } else {
      val bindings = analyzeBindings(table, state, args(0))
      val symbols = bindings.map({ case (s, e) => (s.name.name -> e.eType) }).toMap
      val newState = state.copy(localScopes = symbols :: state.localScopes)
      val expr = newState.analyzeFn(table, newState.copy(atTopLevel = false))(args(1))

      Node.LetN(bindings, expr, expr.eType)
    }
  }

  private def syntaxError(specificMessage: String) = {
    val genericMessage =
      """let expressions must be of the form:
        |  (let BIND EXPR)
        |  where:
        |    BIND is of the form:
        |      (name1 expr1 name2 expr2 namen exprn)
        |    EXPR is any valid expression""".stripMargin

    CompileError.syntaxError(specificMessage, genericMessage)
  }

  private def analyzeBindings(table: SymbolTable, state: State, form: Form): List[(Node.SymLit, Node)] = {
    val rawBindings = form match {
      case Form.ListF(raw) => raw
      case _ => {
        val message = s"""let form expects BIND to be a list of values
                         |  got: ${form}""".stripMargin
        throw syntaxError(message)
      }
    }
    if (rawBindings.length % 2 != 0) {
      val message = s"""let form expects BIND to have an even number of values
                        |  got: ${form}""".stripMargin
      throw syntaxError(message)
    }
    rawBindings.grouped(2).map(analyzeBinding(table, state)).toList
  }

  private def analyzeBinding(table: SymbolTable, state: State)(forms: List[Form]): (Node.SymLit, Node) = {
    if (!forms.head.isInstanceOf[Form.SymF]) {
      val message = s"""BIND form expects NAME to be a SYMBOL
                        |  got: ${forms.head}""".stripMargin
      throw syntaxError(message)
    } else {
      val symbolText = forms(0).asInstanceOf[Form.SymF].value
      val expr = state.analyzeFn(table, state.copy(atTopLevel = false))(forms(1))

      if (SymbolTable.isReservedName(symbolText)) {
        throw CompileError.reservedWordError(symbolText)
      }
      val localName = LocalName(symbolText)
      val symbol = Node.SymLit(localName, expr.eType)

      (symbol, expr)
    }
  }
}
