package flop.backend

import flop.analysis.{ModuleTree, Name, Node}

object Backend {

  trait Expr

  object Expr {
    case object Simple extends Expr
    case object Complex extends Expr
  }

  def emitModule(module: ModuleTree.Module, nodes: List[Node]): List[String] = {
    val state = State(module, 0, 0, 0)

    val requireLines = emitRequires(module)
    val sourceLines = emit(state, nodes)
    val returnLines = emitReturn(module)

    requireLines ++ sourceLines ++ returnLines
  }

  def emit(state: State, nodes: List[Node]): List[String] =
    nodes.map(tryEmit(state))

  private def emitRequires(module: ModuleTree.Module): List[String] = {
    module.imports.map({ case (abbr, full) =>
      val prefix = if (module.name.tree == full.tree) "" else full.tree

      val fullModulePath = (prefix :: full.paths) :+ full.name
      val requireName = localModuleName(fullModulePath)
      val requirePath = fullModulePath.filter(_.nonEmpty).mkString(".")

      s"local ${requireName} = require('${requirePath}')"
    }).toList ++ List("")
  }

  private def emitReturn(module: ModuleTree.Module): List[String] = {
    val names = module.vars.keys.map(name => s"  ${name} = ${name},").toList
    ("" :: "return {" :: names) :+ "}"
  }

  private def tryEmit(state: State)(node: Node): String = node match {
    case Node.TrueLit => "true"
    case Node.FalseLit => "false"
    case Node.NumLit(v) => v.toInt.toString
    case Node.StrLit(v) => "\"%s\"".format(v)
    case Node.SymLit(n, _) => emitName(state, n)
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
    case Name.ModuleName(t, p, n) => emitModuleName(state, t, p, n)
    case Name.TraitFnName(Name.ModuleName(t, p, tn), n) => {
      emitModuleName(state, t, p, n)
    }
  }

  private def emitModuleName(state: State, tree: String, paths: List[String], name: String): String = {
    val currentName = state.module.name
    val currentPaths = currentName.paths :+ currentName.name

    if (currentName.tree == tree && currentPaths == paths) {
      s"${name}"
    } else if (currentName.tree != tree) {
      s"${localModuleName(tree :: paths)}.${name}"
    } else {
      s"${localModuleName(paths)}.${name}"
    }
  }

  private def emitDef(state: State, name: Node.SymLit, value: Node): String = {
    val newState = state.nextVarState()
    val (exprType, emittedValue) = emitExpr(newState, value)

    val result = exprType match {
      // TODO hack for now to not use returned emitExpr result
      case Expr.Simple => s"local ${name.name.name} = ${tryEmit(newState)(value)}"
      case Expr.Complex => {
        s"""local ${newState.varName}
           |${emittedValue}
           |local ${name.name.name} = ${newState.varName}""".stripMargin
      }
    }

    result + "\n"
  }

  private def emitLet(state: State, bindings: Node.Bindings, expr: Node): String = {
    val newState = state.nextVarState()
    val indentedState = newState.indent()

    val (_, bodyClause) = emitExpr(indentedState, expr)
    val emittedBindings = emitBindings(indentedState, bindings)
    s"""${newState.tabStop}local ${newState.varName}
       |${newState.tabStop}do
       |${emitBindings(indentedState, bindings)}
       |${bodyClause}
       |${newState.tabStop}end""".stripMargin
  }

  private def emitIf(state: State, test: Node, ifExpr: Node, elseExpr: Node): String = {
    val newState = state.nextVarState()
    val indentedState = newState.indent()
    val (_, ifClause) = emitExpr(indentedState, ifExpr)
    val (_, elseClause) = emitExpr(indentedState, elseExpr)

    // TODO support code emission for complex testExpr
    s"""${newState.tabStop}local ${newState.varName}
       |${newState.tabStop}if ${tryEmit(newState)(test)} then
       |${ifClause}
       |${newState.tabStop}else
       |${elseClause}
       |${newState.tabStop}end""".stripMargin
  }

  private def emitFunction(state: State, params: Node.Params, expr: Node): String = {
    val newState = state.nextVarState()
    val indentedState = newState.indent()
    val paramNames = params.map(_._1.name.name).mkString(", ")
    val (_, bodyClause) = emitExpr(indentedState, expr)

    s"""${newState.tabStop}function(${paramNames})
       |${indentedState.tabStop}local ${newState.varName}
       |${bodyClause}
       |${indentedState.tabStop}return ${newState.varName}
       |${newState.tabStop}end""".stripMargin
  }

  private def emitLuaApply(state: State, fn: Node.LuaFn, args: List[Node]): String = fn match {
    case Node.LuaIFn(_, n) => emitInfixApply(state, n, args(0), args(1))
    case Node.LuaPFn(_, n) => emitPrefixApply(state, n, args)
  }

  private def emitFlopApply(state: State, name: Name, args: List[Node]): String = {
    emitPrefixApply(state, name, args)
  }

  private def emitExpr(state: State, expr: Node): (Expr, String) = expr match {
    case _:Node.LetN | _:Node.IfN => (Expr.Complex, emitComplexExpr(state, expr))
    case _ => (Expr.Simple, emitSimpleExpr(state, expr))
  }

  private def emitSimpleExpr(state: State, expr: Node): String = {
    s"${state.tabStop}${state.varName} = ${tryEmit(state)(expr)}"
  }

  private def emitComplexExpr(state: State, expr: Node): String = {
    s"""${tryEmit(state)(expr)}
       |${state.tabStop}${state.varName} = ${state.nextVarState().varName}""".stripMargin
  }

  private def emitBindings(state: State, bindings: Node.Bindings): String = {
    bindings.map({ case (sym, expr) =>
      expr match {
        case _:Node.LetN | _:Node.IfN => emitComplexBinding(state, sym, expr)
        case _ => emitSimpleBinding(state, sym, expr)
      }
    }).mkString("\n")
  }

  private def emitSimpleBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    s"${state.tabStop}local ${symbol.name.name} = ${tryEmit(state)(expr)}"
  }

  private def emitComplexBinding(state: State, symbol: Node.SymLit, expr: Node): String = {
    val newState = state.nextVarState()
    s"""${tryEmit(state)(expr)}
       |${state.tabStop}local ${symbol.name.name} = ${state.nextVarState().varName}""".stripMargin
  }

  private def emitInfixApply(state: State, name: Name, left: Node,
      right: Node): String =
    s"(${tryEmit(state)(left)} ${name.name} ${tryEmit(state)(right)})"

  private def emitPrefixApply(state: State, name: Name, args: List[Node]): String =
    args.map(tryEmit(state)).mkString(emitName(state, name) + "(", ", ", ")")

  private def localModuleName(parts: List[String]): String =
    parts.filter(_.nonEmpty).mkString("_")
}
