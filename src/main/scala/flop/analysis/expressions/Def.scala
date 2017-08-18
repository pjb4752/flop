package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Def {

  type FlopError = flop.analysis.Error

  case object NestedDefinitionError extends FlopError {
    val message = "var definitions must occur at the top level"
  }

  case object SyntaxError extends FlopError {
    val message = """def expressions must be of the form:
                    |  (def NAME EXPR)""".stripMargin
  }

  case object BadNameError extends FlopError {
    val message = "def NAME must be a symbol"
  }

  case class DoubleDefineError(name: String) extends FlopError {
    val message = s"def error: cannot redefine var ${name}"
  }

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (!state.atTopLevel) {
      throw CompileError(NestedDefinitionError)
    } else if (args.length != 2) {
      throw CompileError(SyntaxError)
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw CompileError(BadNameError)
    } else {
      val symbolText = args(0).asInstanceOf[Form.SymF].value
      val newState = state.copy(atTopLevel = false)
      val expr = newState.analyzeFn(tree, newState)(args(1))

      if (Core.reserved.contains(symbolText)) {
        throw CompileError(DoubleDefineError(symbolText))
      }
      val symbol = Node.SymLit(symbolText, expr.eType)

      Node.DefN(symbol, expr, expr.eType)
    }
  }
}
