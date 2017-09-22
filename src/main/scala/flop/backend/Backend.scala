package flop.backend

import flop.analysis.{ModuleTree, Name, Node}

object Backend {

  def emitModule(module: ModuleTree.Module, nodes: List[Node]): List[String] = {
    val requireLines = emitRequires(module)
    val sourceLines = emit(nodes)
    val returnLines = emitReturn(module)

    requireLines ++ sourceLines ++ returnLines
  }

  def emit(nodes: List[Node], state: State = State(0, 0, 0)): List[String] =
    nodes.map(tryEmit(state))

  private def emitRequires(module: ModuleTree.Module): List[String] = {
    module.imports.map({ case (abbr, full) =>
      val fullModulePath = full.paths :+ full.name
      val requireName = localModuleName(full.paths :+ full.name)
      val requirePath = fullModulePath.mkString(".")

      s"local ${requireName} = require('${requirePath}')"
    }).toList
  }

  private def emitReturn(module: ModuleTree.Module): List[String] = {
    val names = module.vars.keys.map(name => s"${name} = ${name},").toList
    ("return {" :: names) :+ "}"
  }

  private def tryEmit(state: State)(node: Node): String = node match {
    case Node.TrueLit => "true"
    case Node.FalseLit => "false"
    case Node.NumLit(v) => v.toString
    case Node.StrLit(v) => "\"%s\"".format(v)
    case Node.SymLit(n, _) => emitName(state, n)
    case Node.ListLit(v) => emitList(state, v)
    case Node.MapLit(m) => emitMap(state, m)
    case Node.DefN(n, v, _) => emitDef(state, n, v)
    case Node.LetN(b, e, _) => emitLet(state, b, e)
    case Node.IfN(t, i, e, _) => emitIf(state, t, i, e)
    case Node.FlopFn(_, p, e) => emitFunction(state, p, e)
    case Node.LuaApply(fn, args, _) => emitLuaApply(state, fn, args)
    case Node.FlopApply(n, args, _) => emitFlopApply(state, n, args)
    case Node.TraitN(_, _) => ""
    case Node.TraitImpl(_, _, _) => ""
    case n => throw new Exception(s"emit ${n}???")
  }

  private def emitName(state: State, name: Name): String = name match {
    case lName @ (_:Name.LiteralName | _:Name.LocalName) => lName.name
    case Name.ModuleName(t, p, n) => s"${localModuleName(p)}.${n}"
  }

  // TODO handle complex expression in list members, like let form
  private def emitList(state: State, list: List[Node]): String = {
    val emitFn = tryEmit(state) _
    val members = list.map(emitFn).mkString(", ")

    s"List.new(${members})"
  }

  // TODO handle complex expression in map members, like let form
  private def emitMap(state: State, map: Map[Node, Node]): String = {
    val emitFn = tryEmit(state) _
    val members = map.map({ case (k, v) => s"${emitFn(k)}, ${emitFn(v)}" })

    s"Map.new(${members.mkString(", ")})"
  }

  private def emitDef(state: State, name: Node.SymLit, value: Node): String =
    s"local ${name.name.name} = ${tryEmit(state)(value)}"

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

  private def emitFunction(state: State, params: Node.Params, expr: Node): String = {
    val newState = state.nextVarState()
    val paramNames = params.map(_._1.name.name).mkString(", ")

    s"""function(${paramNames})
       |local ${newState.varName}
       |${emitExpr(newState, expr)}
       |return ${newState.varName}
       |end""".stripMargin
  }

  private def emitLuaApply(state: State, fn: Node.LuaFn, args: List[Node]): String = fn match {
    case Node.LuaIFn(_, n) => emitInfixApply(state, n, args(0), args(1))
    case Node.LuaPFn(_, n) => emitPrefixApply(state, n, args)
  }

  private def emitFlopApply(state: State, name: Name, args: List[Node]): String = {
    emitPrefixApply(state, emitName(state, name), args)
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
    s"local ${symbol.name.name} = ${tryEmit(state)(expr)}"
  }

  private def emitComplexBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    val newState = state.nextVarState()
    s"""${tryEmit(state)(expr)}
       |local ${symbol.name.name} = ${state.nextVarState().varName}""".stripMargin
  }

  private def emitInfixApply(state: State, name: String, left: Node,
      right: Node): String =
    s"(${tryEmit(state)(left)} ${name} ${tryEmit(state)(right)})"

  private def emitPrefixApply(state: State, name: String, args: List[Node]): String =
    args.map(tryEmit(state)).mkString(name + "(", ", ", ")")

  private def localModuleName(parts: List[String]): String = parts.mkString("_")
}
