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

  private def emitLet(state: State, bindings: Node.Bindings, expr: Node): String = {
    val newState = state.incSymbol()
    val bodyClause = emitExpr(newState, expr)

    s"""local ${newState.symbol}
       |do
       |${emitBindings(newState, bindings)}
       |${bodyClause}
       |end""".stripMargin
  }

  private def emitIf(state: State, test: Node, ifExpr: Node, elseExpr: Node): String = {
    val newState = state.incSymbol()
    val ifClause = emitExpr(newState, ifExpr)
    val elseClause = emitExpr(newState, elseExpr)

    s"""local ${newState.symbol}
       |if ${tryEmit(newState)(test)} then
       |${ifClause}
       |else
       |${elseClause}
       |end""".stripMargin
  }

  private def emitExpr(state: State, expr: Node): String = expr match {
    case _:Node.LetN | _:Node.IfN => emitComplexExpr(state, expr)
    case _ => emitSimpleExpr(state, expr)
  }

  private def emitSimpleExpr(state: State, expr: Node): String = {
    s"${state.symbol} = ${tryEmit(state)(expr)}"
  }

  private def emitComplexExpr(state: State, expr: Node): String = {
    s"""${tryEmit(state)(expr)}
       |${state.symbol} = ${state.incSymbol().symbol}""".stripMargin
  }

  private def emitBindings(state: State, bindings: Node.Bindings): String = {
    bindings.map({ case (sym, expr) => expr match {
        case _:Node.LetN | _:Node.IfN => emitComplexBinding(state, sym, expr)
        case _ => emitSimpleBinding(state, sym, expr)
      }
    }).mkString("\n")
  }

  private def emitComplexBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    val newState = state.incSymbol()
    s"""${tryEmit(state)(expr)}
       |local ${symbol.value} = ${state.incSymbol().symbol}""".stripMargin
  }

  private def emitSimpleBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    s"local ${symbol.value} = ${tryEmit(state)(expr)}"
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
