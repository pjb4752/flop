package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._


object TypeExpr {

  def analyze(table: SymbolTable, form: Form): Type = form match {
    case Form.SymF(s) => SymbolTable.lookupTypeLiteral(table, s)
    case t => throw CompileError.undefinedError(t.toString)
  }
}
