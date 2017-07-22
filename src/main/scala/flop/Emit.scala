package flop

object Emit {

  class State(symbolNum: Int, currentIndent: Int) {
    val prefix = "result"
    val tabSize = 2

    def symbol: String = s"${prefix}_${symbolNum}"

    def tabStop: String = " " * (tabSize * currentIndent)

    def incSymbol(): State = State(symbolNum + 1, currentIndent)

    def indent(): State = State(symbolNum, currentIndent + 1)

    def unindent(): State = {
      if (currentIndent < 1) this
      else State(symbolNum, currentIndent - 1)
    }
  }

  object State {
    def apply(symbolNum: Int = 0, currentIndent: Int = 0) =
      new State(symbolNum, currentIndent)
  }

  def emit(nodes: List[Node], state: State = State()): String =
    nodes.map(tryEmit(state)).mkString("\n")

  private def tryEmit(state: State)(node: Node): String = node match {
    case Node.NumLit(v) => v.toString
    case Node.StrLit(v) => "\"%s\"".format(v)
    case Node.SymLit(v) => v
    case Node.DefN(n, v) => emitDef(state, n, v)
    case Node.LetN(b, e) => emitLet(state, b, e)
    case Node.IfN(t, i, e) => emitIf(state, t, i, e)
    case Node.ApplyN(fn, args) => emitApply(state, fn, args)
  }

  private def emitDef(state: State, name: Node.SymLit, value: Node): String =
    s"local ${name.value} = ${tryEmit(state)(value)}"

  private def emitLet(state: State, bindings: List[(Node.SymLit, Node)],
      exprs: Node): String = {
    exprs match {
      case _:Node.NumLit | _:Node.StrLit | _:Node.SymLit | _:Node.ApplyN =>
          emitSimpleLet(state, bindings, exprs)
      case _ => emitComplexLet(state, bindings, exprs)
    }
  }

  private def emitSimpleLet(state: State, bindings: List[(Node.SymLit, Node)],
      exprs: Node): String = {
    val newState = state.incSymbol()
    s"""local ${newState.symbol}
       |do
       |  ${emitBindings(newState, bindings)}
       |  ${newState.symbol} = ${tryEmit(newState)(exprs)}
       |end""".stripMargin
  }

  private def emitComplexLet(state: State, bindings: List[(Node.SymLit, Node)],
      exprs: Node): String = {
    val newState = state.incSymbol()
    s"""local ${newState.symbol}
       |do
       |  ${emitBindings(newState, bindings)}
       |  ${tryEmit(newState)(exprs)}
       |  ${newState.symbol} = ${newState.incSymbol().symbol}
       |end""".stripMargin
  }

  private def emitIf(state: State, test: Node, ifExpr: Node,
      elseExpr: Node): String = {
    val newState = state.incSymbol()
    s"""local ${newState.symbol}
       |if ${tryEmit(newState)(test)} then
       |  ${newState.symbol} = ${tryEmit(newState)(ifExpr)}
       |else
       |  ${newState.symbol} = ${tryEmit(newState)(elseExpr)}
       |end""".stripMargin
  }

  private def emitBindings(state: State,
      bindings: List[(Node.SymLit, Node)]): String = {
    bindings.map(v =>
      s"local ${v._1.value} = ${tryEmit(state)(v._2)}"
    ).mkString("\n  ")
  }

  private def emitApply(state: State, fn: Type.Fn,
      args: List[Node]): String = fn match {
    case Type.InfixFn(n) => emitInfixApply(state, n, args(0), args(1))
    case Type.PrefixFn(n, _) => emitPrefixApply(state, n, args)
  }

  private def emitInfixApply(state: State, name: String, left: Node,
      right: Node): String =
    s"(${tryEmit(state)(left)} ${name} ${tryEmit(state)(right)})"

  private def emitPrefixApply(state: State, name: String,
      args: List[Node]): String =
    args.map(tryEmit(state)).mkString(name + "(", ", ", ")")
}
