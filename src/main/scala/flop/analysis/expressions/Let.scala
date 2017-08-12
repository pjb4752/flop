package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Let {

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid let form, expected (let BIND EXPR)")
    } else {
      val bindings = analyzeBindings(tree, state, args(0))
      val symbols = bindings.map({ case (s, e) => (s.value -> e.eType) }).toMap
      val newState = state.copy(localScopes = symbols :: state.localScopes)
      val expr = newState.analyzeFn(tree, newState.copy(atTopLevel = false))(args(1))

      Node.LetN(bindings, expr, expr.eType)
    }
  }

  private def analyzeBindings(tree: ModuleTree, state: State, form: Form): List[(Node.SymLit, Node)] = {
    val rawBindings = form match {
      case Form.ListF(raw) => raw
      case _ => throw CompileError("BIND must be a list of SYM EXPR pairs")
    }
    if (rawBindings.length % 2 != 0) {
      throw CompileError("BIND must have even number of terms")
    }
    rawBindings.grouped(2).map(analyzeBinding(tree, state)).toList
  }

  private def analyzeBinding(tree: ModuleTree, state: State)(forms: List[Form]): (Node.SymLit, Node) = {
    if (!forms.head.isInstanceOf[Form.SymF]) {
      throw CompileError("BIND expects first value to be a name")
    } else {
      val symbolText = forms(0).asInstanceOf[Form.SymF].value
      val expr = state.analyzeFn(tree, state.copy(atTopLevel = false))(forms(1))
      if (Core.reserved.contains(symbolText)) {
        throw CompileError(s"cannot redefine ${symbolText}")
      }
      val symbol = Node.SymLit(symbolText, expr.eType)

      (symbol, expr)
    }
  }
}
