package flop.analysis

import flop.reading.Form
import flop.stdlib.Core

object SymbolTable {

  def lookupType(tree: ModuleTree, state: State, name: String): Type = {
    val localBind = state.localScopes.find(_.contains(name))

    if (localBind.nonEmpty) {
      localBind.get(name)
    // lookup in module tree
    //} else if (state.currentModule.vars.contains(name)) {
      //state.currentModule.vars(name).eType
    } else if (Core.builtins.contains(name)) {
      Core.builtins(name).eType
    } else {
      val maybeFn = findTraitFnType(tree, state, name)

      if (maybeFn.nonEmpty) {
        maybeFn.get
      } else {
        throw CompileError(s"symbol ${name} not found")
      }
    }
  }

  def findTraitFnDef(tree: ModuleTree, state: State, name: String): Option[Node.FnDef] = {
    Core.builtinTraits.flatMap(_._2).
        find({ case Node.FnDef(_, n, _) => n.value == name })
  }

  def findTraitFnType(tree: ModuleTree, state: State, name: String): Option[Type] = {
    findTraitFnDef(tree, state, name).map({ case Node.FnDef(_, _, t) => t })
  }

  def findTraitImpl(tree: ModuleTree, state: State, name: String, selfType: Type):
      Option[Node.TraitImpl] = {
    Core.traitImpls.get(name).flatMap(_.get(selfType))
  }

  // TODO put this elsewhere
  def analyzeTypeForm(tree: ModuleTree, form: Form): Type = form match {
    case Form.SymF(s) => analyzeTypeLiteral(tree, s)
    case t => throw CompileError(s"unknown type: ${t}")
  }

  def analyzeTypeLiteral(tree: ModuleTree, lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "bool" => Type.Boolean
    case "num" => Type.Number
    case "str" => Type.String
    case "sym" => Type.Symbol
    case t => throw CompileError(s"unknown type: ${t}")
  }

}
