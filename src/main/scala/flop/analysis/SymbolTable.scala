package flop.analysis

import flop.analysis.ModuleTree._
import flop.analysis.ModuleTree.Module._
import flop.analysis.Name._
import flop.reading.Form
import flop.stdlib.Core

object SymbolTable {

  type FlopError = flop.analysis.Error

  case class NameValueError(form: String) extends FlopError {
    val message = s"cannot take value of special form ${form}"
  }

  case class UndefinedError(name: String) extends FlopError {
    val message = s"symbol ${name} not found"
  }

  case class BadModuleError(paths: List[String]) extends FlopError {
    val message = s"""module ${paths.mkString(".")} not found"""
  }

  case class UnknownTypeError(typeName: String) extends FlopError {
    val message = s"unknown type ${typeName}"
  }

  case class InvalidVarName(name: String) extends FlopError {
    val message = s"invalid var name ${name}"
  }

  /*
   * For right now these are special 'syntax' recognized by the compiler,
   * that don't follow standard rules of application
   */
  val specialForms = List("def", "fn", "if", "let")

  def isSpecialForm(s: String) = specialForms.contains(s)

  val literals = Map(
    "true" -> Node.TrueLit,
    "false" -> Node.FalseLit
  )

  def isLiteral(s: String) = literals.contains(s)

  def literalValue(s: String) = literals(s)

  def reservedNames = specialForms ++ literals.keys.toList

  def isReservedName(s: String) = reservedNames.contains(s)

  def isValidPath(tree: ModuleTree, paths: List[String]): Boolean =
    ModuleTree.isValidPath(tree, paths)

  def lookupName(tree: ModuleTree, state: State, raw: String): Option[Name] = {
    if (isLiteral(raw)) {
      Some(LiteralName(raw))
    } else if (raw.contains('.')) {
      lookupQualifiedName(tree, state, raw)
    } else {
      lookupUnqualifiedName(tree, state, raw)
    }
  }

  def lookupVar(tree: ModuleTree, name: Name): Option[Var] = name match {
    case m: ModuleName => lookupModuleVar(tree, m)
    case _ => throw CompileError(InvalidVarName(name.toString))
  }

  def lookupQualifiedName(tree: ModuleTree, state: State, raw: String): Option[Name] = {
    val nameParts = raw.split("\\.").toList

    if (SymbolTable.isReservedName(nameParts.last)) {
      throw CompileError(NameValueError(nameParts.last))
    } else if (!SymbolTable.isValidPath(tree, nameParts)) {
      throw CompileError(BadModuleError(nameParts))
    } else {
      val paths = nameParts.slice(1, nameParts.length - 1)
      val name = ModuleName(nameParts.head, paths, nameParts.last)

      Some(name)
    }
  }

  def lookupUnqualifiedName(tree: ModuleTree, state: State, raw: String): Option[Name] = {
    if (SymbolTable.isReservedName(raw)) {
      throw CompileError(NameValueError(raw))
    }

    val nameParts = lookupRawName(tree, state, raw)
    if (nameParts.isEmpty) {
      throw CompileError(UndefinedError(raw))
    } else {
      nameParts
    }
  }

  def lookupType(tree: ModuleTree, state: State, name: Name): Type = name match {
    case m: ModuleName => lookupModuleNameType(tree, m)
    case l: LiteralName => lookupLiteralType(tree, l)
    case l: LocalName => lookupLocalType(state, l)
    case _ => throw CompileError(UnknownTypeError(name.name))
  }

  private def lookupRawName(tree: ModuleTree, state: State, raw: String): Option[Name] = {
    val localBinding = state.localScopes.find(_.contains(raw))

    if (localBinding.nonEmpty) {
      Some(LocalName(raw))
    } else {
      val currentModule = if (state.currentPaths.isEmpty) {
        tree.children.get(state.currentTree).map(_.asInstanceOf[Module])
      } else {
        ModuleTree.findModule(tree, state.currentPaths)
      }

      val name = currentModule.flatMap(maybeMod => maybeMod.vars.get(raw))

      name.map(maybeName =>
        ModuleName(state.currentTree, state.currentPaths, maybeName.name))
    }
  }

  private def lookupModuleNameType(tree: ModuleTree, name: ModuleName): Type = {
    val moduleType = lookupModuleType(tree, name)

    if (moduleType.nonEmpty) {
      moduleType.get
    } else {
      throw CompileError(UndefinedError(name.toString))
    }
    // TODO put this back when trait lookup is handled
    //} else {
      //val fnType = lookupTraitFnType(tree, name)

      //if (fnType.nonEmpty) {
        //fnType.get
      //} else {
        //throw CompileError(UndefinedError(name.toString))
      //}
    //}
  }

  private def lookupLiteralType(tree: ModuleTree, name: LiteralName): Type = {
    val literal = literals.get(name.name)

    if (literal.nonEmpty) {
      literal.get.eType
    } else {
      throw CompileError(UndefinedError(name.name))
    }
  }

  private def lookupLocalType(state: State, name: LocalName): Type = {
    val localBinding = state.localScopes.find(_.contains(name.name))

    if (localBinding.nonEmpty) {
      localBinding.get(name.name)
    } else {
      throw CompileError(UndefinedError(name.name))
    }
  }

  private def lookupModuleVar(tree: ModuleTree, name: ModuleName): Option[Var] = {
    ModuleTree.findModule(tree, name.paths).flatMap(maybeModule =>
      maybeModule.vars.get(name.name))
  }

  private def lookupModuleType(tree: ModuleTree, name: ModuleName): Option[Type] = {
    ModuleTree.findModule(tree, name.paths).flatMap(maybeModule =>
      maybeModule.vars.get(name.name).map(_.node.eType))
  }

  // TODO figure out how to handle this, should traits have unique function names
  // per module or should trait 'namespace' the function names?
  //private def lookupTraitFnType(tree: ModuleTree, name: ModuleName): Option[Type] = {
    //ModuleTree.findModule(tree, name.paths).flatMap(maybeModule =>
      //maybeModule.traits.get(name.name))
  //}

  // TODO handle trait implementation lookup
  //def findTraitImpl(tree: ModuleTree, state: State, name: ModuleName, selfType: Type): Option[Node.TraitImpl] = {
    //ModuleTree.findModule(tree, name.paths).flatMap(maybeModule =>
      //maybeModule.traits.get(name.name).
  //}

  // TODO put this elsewhere
  def analyzeTypeForm(tree: ModuleTree, form: Form): Type = form match {
    case Form.SymF(s) => analyzeTypeLiteral(tree, s)
    case t => throw CompileError(UnknownTypeError(t.toString))
  }

  def analyzeTypeLiteral(tree: ModuleTree, lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "bool" => Type.Boolean
    case "num" => Type.Number
    case "str" => Type.String
    case "sym" => Type.Symbol
    case t => throw CompileError(UnknownTypeError(t))
  }
}
