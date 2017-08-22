package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object ListExpr {

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    val analyzeFn = state.analyzeFn(table, state.copy(atTopLevel = false))
    Node.ListLit(args.map(analyzeFn))
  }
}
