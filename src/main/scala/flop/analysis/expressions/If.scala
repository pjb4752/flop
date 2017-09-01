package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object If {

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      val message = s"""Wrong number of terms in if form"
                       |  expected: 3, actual: ${args.length}""".stripMargin
      throw syntaxError(message)
    } else {
      val newState = state.copy(atTopLevel = false)
      val mapFn = newState.analyzeFn(table, newState)

      val analyzed = args.map(mapFn)
      val testType = analyzed(0).eType
      val ifType = analyzed(1).eType
      val elseType = analyzed(2).eType

      if (testType != Type.Boolean) {
        throw CompileError.typeError("if TEST-EXPR", Type.Boolean, testType)
      } else if (ifType != elseType) {
        throw CompileError.typeError("if ELSE-EXPR", ifType, elseType)
      }

      Node.IfN(analyzed(0), analyzed(1), analyzed(2), ifType)
    }
  }

  private def syntaxError(specificMessage: String) = {
    val genericMessage =
      """if expressions must be of the form:
        |  (if TEST IF-EXPR ELSE-EXPR)
        |  where:
        |    TEST is any valid expression returning a boolean value
        |    IF-EXPR is any valid expression
        |    ELSE-EXPR is andy valid expression""".stripMargin

    CompileError.syntaxError(specificMessage, genericMessage)
  }
}
