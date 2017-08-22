package flop.analysis

import flop.analysis.ModuleTree._
import flop.analysis.ModuleTree.Module._
import flop.analysis.Name._
import flop.reading.Form
import flop.stdlib.Core

case class SymbolTable(trees: Map[String, ModuleTree])

object SymbolTable {

  def withRoot(rootName: String) = {
    val moduleTree = ModuleTree.newRoot(rootName)
    val initialMap = Map(rootName -> moduleTree)
    val coreLib = Core.library
    val withStdLib = initialMap + (coreLib.name -> coreLib)

    SymbolTable(withStdLib)
  }

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

  def lookupName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    if (isLiteral(raw)) {
      Some(LiteralName(raw))
    } else if (raw.contains('.')) {
      lookupQualifiedName(table, state, raw)
    } else {
      lookupUnqualifiedName(table, state, raw)
    }
  }

  def lookupVar(table: SymbolTable, name: Name): Option[Var] = name match {
    case m: ModuleName => lookupModuleVar(table, m)
    case _ => throw CompileError(InvalidVarName(name.toString))
  }

  def lookupQualifiedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val nameParts = raw.split("\\.").toList

    if (SymbolTable.isReservedName(nameParts.last)) {
      throw CompileError(NameValueError(nameParts.last))
    }

    val treeName = nameParts.head
    val paths = nameParts.slice(1, nameParts.length - 1)

    val validPath = table.trees.get(treeName).map(tree =>
      SymbolTable.isValidPath(tree, paths)).getOrElse(false)

    if (!validPath) {
      throw CompileError(BadModuleError(nameParts))
    }

    Some(ModuleName(nameParts.head, paths, nameParts.last))
  }

  def lookupUnqualifiedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    if (SymbolTable.isReservedName(raw)) {
      throw CompileError(NameValueError(raw))
    }

    val nameParts = lookupRawName(table, state, raw)
    if (nameParts.isEmpty) {
      throw CompileError(UndefinedError(raw))
    } else {
      nameParts
    }
  }

  def lookupType(table: SymbolTable, state: State, name: Name): Type = name match {
    case m: ModuleName => lookupModuleNameType(table, m)
    case l: LiteralName => lookupLiteralType(table, l)
    case l: LocalName => lookupLocalType(state, l)
    case _ => throw CompileError(UnknownTypeError(name.name))
  }

  private def lookupRawName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val localBinding = state.localScopes.find(_.contains(raw))

    if (localBinding.nonEmpty) {
      Some(LocalName(raw))
    } else {
      // first we look in the current module
      val name = table.trees.get(state.currentTree).flatMap(maybeTree =>
          ModuleTree.findModule(maybeTree, state.currentPaths)).
        flatMap(maybeMod => maybeMod.vars.get(raw))

      if (name.nonEmpty) {
        Some(ModuleName(state.currentTree, state.currentPaths, name.get.name))
      } else {
        lookupGlobalName(table, state, raw)
      }
    }
  }

  private def lookupGlobalName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val coreTree = table.trees(Core.coreName)
    val commonModule = ModuleTree.findModule(coreTree, Core.commonPath)

    commonModule.flatMap(maybeMod => maybeMod.vars.get(raw)).map(v =>
      ModuleName(Core.coreName, Core.commonPath, v.name))
  }

  private def lookupModuleNameType(table: SymbolTable, name: ModuleName): Type = {
    val moduleType = lookupModuleType(table, name)

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

  private def lookupLiteralType(table: SymbolTable, name: LiteralName): Type = {
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

  private def lookupModuleVar(table: SymbolTable, name: ModuleName): Option[Var] = {
    table.trees.get(name.tree).flatMap(tree => ModuleTree.findModule(tree, name.paths)).
      flatMap(maybeModule => maybeModule.vars.get(name.name))
  }

  private def lookupModuleType(table: SymbolTable, name: ModuleName): Option[Type] = {
    table.trees.get(name.tree).flatMap(tree => ModuleTree.findModule(tree, name.paths)).
      flatMap(maybeMod => maybeMod.vars.get(name.name).map(_.node.eType))
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
  def analyzeTypeForm(table: SymbolTable, form: Form): Type = form match {
    case Form.SymF(s) => analyzeTypeLiteral(table, s)
    case t => throw CompileError(UnknownTypeError(t.toString))
  }

  def analyzeTypeLiteral(table: SymbolTable, lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "bool" => Type.Boolean
    case "num" => Type.Number
    case "str" => Type.String
    case "sym" => Type.Symbol
    case t => throw CompileError(UnknownTypeError(t))
  }
}
