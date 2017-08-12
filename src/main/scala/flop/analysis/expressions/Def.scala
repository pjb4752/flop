package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Def {

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid def form, expected (def SYM EXPR)")
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw CompileError("def expects first arg to be a name")
    } else if (!state.atTopLevel) {
      throw CompileError("def must occur at module (top) level")
    } else {
      val symbolText = args(0).asInstanceOf[Form.SymF].value
      val expr = state.analyzeFn(tree, state.copy(atTopLevel = false))(args(1))

      if (Core.reserved.contains(symbolText)) {
        throw CompileError(s"cannot redefine ${symbolText}")
      }
      val symbol = Node.SymLit(symbolText, expr.eType)

      Node.DefN(symbol, expr, expr.eType)
    }
  }
}
