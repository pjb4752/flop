package flop

object Emit {

  def emit(nodes: List[Node], symbolTable: SymbolTable): String =
    nodes.map(tryEmit(symbolTable)).mkString("\n")

  private def tryEmit(symbolTable: SymbolTable)(node: Node): String = node match {
    case Node.NumLit(s, v) => v.toString
    case Node.StrLit(s, v) => "\"%s\"".format(v)
    case Node.SymLit(s, v) => v
    case Node.DefN(s, n, v) => emitDef(symbolTable, n, v)
    case Node.LetN(s, b, e) => emitLet(symbolTable, b, e)
    case Node.IfN(s, t, i, e) => emitIf(symbolTable, t, i, e)
    case Node.ApplyN(s, fn, args) => emitApply(symbolTable, fn, args)
  }

  private def emitDef(symbolTable: SymbolTable, name: Node.SymLit,
      value: Node): String =
    s"local ${name.value} = ${tryEmit(symbolTable)(value)}"

  private def emitLet(symbolTable: SymbolTable,
      bindings: List[(Node.SymLit, Node)], exprs: Node): String = {
    val symbol = symbolTable.genSymbol()
    """local %s
      |do
      |  %s
      |  %s = %s
      |end""".stripMargin.format(symbol, emitBindings(symbolTable, bindings),
        symbol, tryEmit(symbolTable)(exprs))
  }

  private def emitIf(symbolTable: SymbolTable, test: Node, ifExpr: Node,
      elseExpr: Node): String = {
    val symbol = symbolTable.genSymbol()
    """local %s
      |if %s then
      |  %s = %s
      |else
      |  %s = %s
      |end""".stripMargin.format(symbol, tryEmit(symbolTable)(test), symbol,
        tryEmit(symbolTable)(ifExpr), symbol, tryEmit(symbolTable)(elseExpr))
  }

  private def emitBindings(symbolTable: SymbolTable,
      bindings: List[(Node.SymLit, Node)]): String =
    bindings.map(v =>
      s"local ${v._1.value} = ${tryEmit(symbolTable)(v._2)}"
    ).mkString("\n  ")

  private def emitApply(symbolTable: SymbolTable, fn: Type.Fn,
      args: List[Node]): String = fn match {
    case Type.InfixFn(n) => emitInfixApply(symbolTable, n, args(0), args(1))
    case Type.PrefixFn(n, _) => emitPrefixApply(symbolTable, n, args)
  }

  private def emitInfixApply(symbolTable: SymbolTable, name: String,
      left: Node, right: Node): String =
    s"(${tryEmit(symbolTable)(left)} ${name} ${tryEmit(symbolTable)(right)})"

  private def emitPrefixApply(symbolTable: SymbolTable, name: String,
      args: List[Node]): String =
    args.map(tryEmit(symbolTable)).mkString(name + "(", ", ", ")")
}
