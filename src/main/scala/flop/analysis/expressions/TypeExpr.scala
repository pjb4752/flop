package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, SymbolTable, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._


object TypeExpr {

  // TODO put this elsewhere
  def analyzeTypeForm(tree: ModuleTree, form: Form): Type = form match {
    case Form.SymF(s) => SymbolTable.analyzeTypeLiteral(tree, s)
    case t => throw CompileError(SymbolTable.UnknownTypeError(t.toString))
  }
}
