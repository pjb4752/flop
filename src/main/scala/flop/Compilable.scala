package flop

trait Compilable {

  def compile(symbolTable: SymbolTable)(source: String): String =
    Emit.emit(Analyze.analyze(Read.read(source)), symbolTable)
}
