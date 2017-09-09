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

  def findModule(table: SymbolTable, tree: String, paths: List[String]): Option[Module] = {
    table.trees.get(tree).flatMap(t => ModuleTree.findModule(t, paths))
  }

  def isLoaded(table: SymbolTable, tree: String, paths: List[String]): Boolean = {
    findModule(table, tree, paths).nonEmpty
  }

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
    case _ => {
      val message = s"${name} is invalid var name"
      throw CompileError.nameError(message)
    }
  }

  def lookupQualifiedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val nameParts = raw.split("\\.").toList

    if (SymbolTable.isReservedName(nameParts.last)) {
      throw CompileError.reservedWordError(nameParts.last)
    }

    val treeName = nameParts.head
    val paths = nameParts.slice(1, nameParts.length - 1)

    val validPath = table.trees.get(treeName).map(tree =>
      SymbolTable.isValidPath(tree, paths)).getOrElse(false)

    if (!validPath) {
      val message = s"Module name ${nameParts} is not valid"
      throw CompileError.moduleError(message)
    }

    // first we look in the current module
    val name = ModuleName(nameParts.head, paths, nameParts.last)
    lookupVar(table, name).map(_ => name)
  }

  def lookupUnqualifiedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    if (SymbolTable.isReservedName(raw)) {
      throw CompileError.reservedWordError(raw)
    }

    lookupRawName(table, state, raw)
  }

  def lookupType(table: SymbolTable, state: State, name: Name): Type = name match {
    case m: ModuleName => lookupModuleNameType(table, m)
    case l: LiteralName => lookupLiteralType(table, l)
    case l: LocalName => lookupLocalType(state, l)
    case _ => throw CompileError.undefinedError(name.name)
  }

  private def lookupRawName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val localBinding = state.localScopes.find(_.contains(raw))

    if (localBinding.nonEmpty) {
      Some(LocalName(raw))
    } else {
      // first we look in the current module
      val name = state.currentModule.vars.get(raw)

      if (name.nonEmpty) {
        Some(Name.ModuleName.nest(state.currentModule.name, raw))
      } else {
        lookupGlobalName(table, state, raw)
      }
    }
  }

  private def lookupGlobalName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val flopTree = table.trees(Core.rootName)
    val commonModule = ModuleTree.findModule(flopTree, Core.commonPath)

    commonModule.flatMap(maybeMod => maybeMod.vars.get(raw)).map(v =>
      ModuleName(Core.rootName, Core.commonPath, v.name))
  }

  private def lookupModuleNameType(table: SymbolTable, name: ModuleName): Type = {
    val moduleType = lookupModuleType(table, name)

    if (moduleType.nonEmpty) {
      moduleType.get
    } else {
      throw CompileError.undefinedError(name.toString)
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
      throw CompileError.undefinedError(name.name)
    }
  }

  private def lookupLocalType(state: State, name: LocalName): Type = {
    val localBinding = state.localScopes.find(_.contains(name.name))

    if (localBinding.nonEmpty) {
      localBinding.get(name.name)
    } else {
      throw CompileError.undefinedError(name.name)
    }
  }

  private def lookupModuleVar(table: SymbolTable, name: ModuleName): Option[Var] = {
    findModule(table, name.tree, name.paths).
      flatMap(maybeMod => maybeMod.vars.get(name.name))
  }

  private def lookupModuleType(table: SymbolTable, name: ModuleName): Option[Type] = {
    findModule(table, name.tree, name.paths).
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
    case t => throw CompileError.undefinedError(t.toString)
  }

  def analyzeTypeLiteral(table: SymbolTable, lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "bool" => Type.Boolean
    case "num" => Type.Number
    case "str" => Type.String
    case "sym" => Type.Symbol
    case t => throw CompileError.undefinedError(t)
  }

  def addModule(table: SymbolTable, module: Module): SymbolTable = {
    val blankTree = ModuleTree.newRoot(module.name.tree)
    val tree = table.trees.getOrElse(module.name.tree, blankTree)
    val newTree = ModuleTree.addModule(tree, module)

    table.copy(trees = table.trees + (newTree.name -> tree))
  }
}
