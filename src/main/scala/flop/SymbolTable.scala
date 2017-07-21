package flop

trait SymbolTable {
  def genSymbol(prefix: String = "result"): String
}

class CompilerSymbolTable extends SymbolTable {
  var value = 0

  override def genSymbol(prefix: String = "result") = {
    value += 1
    s"${prefix}_${value}"
  }
}
