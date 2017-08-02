package flop

trait Compilable {

  def compile(analyzeState: Analyze.State, emitState: Emit.State)(source: String): String = {
    val forms = Read.read(source)
    val (newState, ast) = Analyze.analyze(analyzeState, forms)

    Emit.emit(ast, emitState)
  }
}
