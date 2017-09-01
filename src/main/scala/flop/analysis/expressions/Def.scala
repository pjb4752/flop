package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Def {

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    if (!state.atTopLevel) {
      val message =
        """Unexpected def form in a nested scope
          |  var definitions must occur at a module top level""".stripMargin
      throw syntaxError(message)
    } else if (args.length != 2) {
      val message = s"""Wrong number of terms in def form"
                       |  expected: 2, actual: ${args.length}""".stripMargin
      throw syntaxError(message)
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      val message = s"""def form expects first term to be a SYMBOL, got:
                       |  ${args.head}""".stripMargin
      throw syntaxError(message)
    } else {
      val symbolText = args(0).asInstanceOf[Form.SymF].value
      val newState = state.copy(atTopLevel = false)
      val expr = newState.analyzeFn(table, newState)(args(1))

      if (symbolText.contains(".")) {
        val message = s"""def form expects NAME to be unqualified, got:
                         |  ${symbolText}""".stripMargin
        throw syntaxError(message)
      } else if (SymbolTable.isReservedName(symbolText)) {
        throw CompileError.reservedWordError(symbolText)
      }

      val name = ModuleName(state.currentTree, state.currentPaths, symbolText)
      val symbol = Node.SymLit(name, expr.eType)

      Node.DefN(symbol, expr, expr.eType)
    }
  }

  private def syntaxError(specificMessage: String) = {
    val genericMessage =
      """var definitions must be of the form:
        |  (def NAME EXPR)
        |  where:
        |    NAME is an unqualified SYMBOL
        |    EXPR is any valid expression that returns a result""".stripMargin

    CompileError.syntaxError(specificMessage, genericMessage)
  }
}
