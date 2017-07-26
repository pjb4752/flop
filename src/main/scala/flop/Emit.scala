package flop

object Emit {

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

  def emit(nodes: List[Node], state: State = State(0, 0, 0)): String =
    nodes.map(tryEmit(state)).mkString("\n")

  private def tryEmit(state: State)(node: Node): String = node match {
    case Node.NumLit(v) => v.toString
    case Node.StrLit(v) => "\"%s\"".format(v)
    case Node.SymLit(v) => v
    case Node.DefN(n, v) => emitDef(state, n, v)
    case Node.LetN(b, e) => emitLet(state, b, e)
    case Node.IfN(t, i, e) => emitIf(state, t, i, e)
    case Node.FnN(b, e) => emitFn(state, b, e)
    case Node.ApplyN(fn, args) => emitApply(state, fn, args)
  }

  private def emitDef(state: State, name: Node.SymLit, value: Node): String =
    s"local ${name.value} = ${tryEmit(state)(value)}"

  private def emitLet(state: State, bindings: Node.Bindings, expr: Node): String = {
    val newState = state.nextVarState()
    val bodyClause = emitExpr(newState, expr)

    s"""local ${newState.varName}
       |do
       |${emitBindings(newState, bindings)}
       |${bodyClause}
       |end""".stripMargin
  }

  private def emitIf(state: State, test: Node, ifExpr: Node, elseExpr: Node): String = {
    val newState = state.nextVarState()
    val ifClause = emitExpr(newState, ifExpr)
    val elseClause = emitExpr(newState, elseExpr)

    // TODO support code emission for complex testExpr
    s"""local ${newState.varName}
       |if ${tryEmit(newState)(test)} then
       |${ifClause}
       |else
       |${elseClause}
       |end""".stripMargin
  }

  private def emitFn(state: State, names: List[Node.SymLit], expr: Node): String = {
    val newState = state.nextVarState()
    val paramNames = names.map(_.value).mkString(", ")

    s"""function(${paramNames})
       |local ${newState.varName}
       |${emitExpr(newState, expr)}
       |return ${newState.varName}
       |end""".stripMargin
  }

  private def emitApply(state: State, fn: Type.Fn,
      args: List[Node]): String = fn match {
    case Type.InfixFn(n) => emitInfixApply(state, n, args(0), args(1))
    case Type.PrefixFn(n, _) => emitPrefixApply(state, n, args)
  }

  private def emitExpr(state: State, expr: Node): String = expr match {
    case _:Node.LetN | _:Node.IfN => emitComplexExpr(state, expr)
    case _ => emitSimpleExpr(state, expr)
  }

  private def emitSimpleExpr(state: State, expr: Node): String = {
    s"${state.varName} = ${tryEmit(state)(expr)}"
  }

  private def emitComplexExpr(state: State, expr: Node): String = {
    s"""${tryEmit(state)(expr)}
       |${state.varName} = ${state.nextVarState().varName}""".stripMargin
  }

  private def emitBindings(state: State, bindings: Node.Bindings): String = {
    bindings.map({ case (sym, expr) => expr match {
        case _:Node.LetN | _:Node.IfN => emitComplexBinding(state, sym, expr)
        case _ => emitSimpleBinding(state, sym, expr)
      }
    }).mkString("\n")
  }

  private def emitSimpleBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    s"local ${symbol.value} = ${tryEmit(state)(expr)}"
  }

  private def emitComplexBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    val newState = state.nextVarState()
    s"""${tryEmit(state)(expr)}
       |local ${symbol.value} = ${state.nextVarState().varName}""".stripMargin
  }

  private def emitInfixApply(state: State, name: String, left: Node,
      right: Node): String =
    s"(${tryEmit(state)(left)} ${name} ${tryEmit(state)(right)})"

  private def emitPrefixApply(state: State, name: String,
      args: List[Node]): String =
    args.map(tryEmit(state)).mkString(name + "(", ", ", ")")
}
