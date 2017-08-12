package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object ListExpr {

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    val analyzeFn = state.analyzeFn(tree, state.copy(atTopLevel = false))
    Node.ListLit(args.map(analyzeFn))
  }
}
