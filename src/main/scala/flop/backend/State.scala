package flop.backend

case class State(varNum: Int, fnNum: Int, currentIndent: Int) {
  val varPrefix = "var"
  val fnPrefix = "fn"
  val tabSize = 2

  def varName: String = s"${varPrefix}_${varNum}"

  def fnName: String = s"${fnPrefix}_${fnNum}"

  def tabStop: String = " " * (tabSize * currentIndent)

  def nextVarState(): State = this.copy(varNum = this.varNum + 1)

  def nextFnState(): State = this.copy(fnNum = this.fnNum + 1)

  def indent(): State = this.copy(currentIndent = this.currentIndent + 1)

  def unindent(): State = {
    if (currentIndent < 1) this
    else State(varNum, fnNum, currentIndent - 1)
  }
}

object State {

  def initial: State = {
    State(0, 0, 0)
  }
}
