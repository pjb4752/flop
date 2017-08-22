package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._


object TypeExpr {

  // TODO put this elsewhere
  def analyzeTypeForm(table: SymbolTable, form: Form): Type = form match {
    case Form.SymF(s) => SymbolTable.analyzeTypeLiteral(table, s)
    case t => throw CompileError(SymbolTable.UnknownTypeError(t.toString))
  }
}
