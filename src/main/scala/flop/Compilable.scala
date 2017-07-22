package flop

trait Compilable {

  def compile(state: Emit.State)(source: String): String =
    Emit.emit(Analyze.analyze(Read.read(source)), state)
}
