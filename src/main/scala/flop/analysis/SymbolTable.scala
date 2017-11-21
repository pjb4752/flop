package flop.analysis

import flop.analysis.ModuleTree._
import flop.analysis.ModuleTree.Module._
import flop.analysis.Name._
import flop.reading.Form
import flop.stdlib.Core
import flop.stdlib.core.Common

case class SymbolTable(trees: Map[String, ModuleTree])

object SymbolTable {

  val specialForms = List("def", "fn", "if", "let")

  val selfLiteral = "self"
  val selfType = Type.Self

  val literals = Map(
    "true" -> Node.TrueLit,
    "false" -> Node.FalseLit
  )

  def withStdLib() = {
    val coreLib = Core.library
    val initialMap = Map(coreLib.name -> coreLib)

    SymbolTable(initialMap)
  }

  def withRoot(rootName: String) = {
    val moduleTree = ModuleTree.newRoot(rootName)
    val initialMap = Map(rootName -> moduleTree)
    val coreLib = Core.library
    val withStdLib = initialMap + (coreLib.name -> coreLib)

    SymbolTable(withStdLib)
  }

  def isSpecialForm(s: String) = specialForms.contains(s)

  def isSelfLiteral(s: String) = s == selfLiteral

  def isLiteral(s: String) = literals.contains(s)

  def literalValue(s: String) = literals(s)

  def reservedNames = specialForms ++ literals.keys.toList

  def isReservedName(s: String) = reservedNames.contains(s)

  def isValidPath(tree: ModuleTree, paths: List[String]): Boolean =
    ModuleTree.isValidPath(tree, paths)

  def findModule(table: SymbolTable, tree: String, paths: List[String]): Option[Module] = {
    table.trees.get(tree).flatMap(t => ModuleTree.findModule(t, paths))
  }

  def getModule(table: SymbolTable, moduleName: Name.ModuleName): Module = {
      val Name.ModuleName(tree, paths, name) = moduleName
      // current module should always exist
      findModule(table, tree, paths :+ name).get
  }

  def addModule(table: SymbolTable, module: Module): SymbolTable = {
    val blankTree = ModuleTree.newRoot(module.name.tree)
    val tree = table.trees.getOrElse(module.name.tree, blankTree)
    val newTree = ModuleTree.addModule(tree, module)

    table.copy(trees = table.trees + (newTree.name -> newTree))
  }

  def updateModule(table: SymbolTable, module: Module): SymbolTable = {
    val tree = table.trees.get(module.name.tree)

    if (tree.nonEmpty) {
      val newTree = ModuleTree.updateModule(tree.get, module)
      table.copy(trees = table.trees + (newTree.name -> newTree))
    } else {
      val message = s"Module path is invalid: ${module.name}"
      throw CompileError.moduleError(message)
    }
  }

  def isLoaded(table: SymbolTable, tree: String, paths: List[String]): Boolean =
    findModule(table, tree, paths).nonEmpty

  def lookupName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    if (isLiteral(raw)) {
      Some(LiteralName(raw))
    } else if (raw.contains('/')) {
      if (raw.contains('.')) {
        lookupQualifiedName(table, state, raw)
      } else {
        lookupAliasedName(table, state, raw)
      }
    } else {
      if (raw.contains('.')) {
        val message = s"invalid name format: ${raw}, only module name given"
        throw CompileError.nameError(message)
      } else {
        lookupUnqualifiedName(table, state, raw)
      }
    }
  }

  // TODO this should return None?
  def lookupType(table: SymbolTable, state: State, name: Name): Type = name match {
    case l: LiteralName => lookupLiteralType(table, l)
    case l: LocalName => lookupLocalType(state, l)
    case m: ModuleName => lookupModuleType(table, m)
    case f: TraitFnName => lookupTraitFnType(table, f)
    case _ => throw CompileError.undefinedError(name.name)
  }

  def lookupTypeLiteral(table: SymbolTable, lit: String): Type = lit match {
    case "unit" => Type.Unit
    case "bool" => Type.Boolean
    case "num" => Type.Number
    case "str" => Type.String
    case "sym" => Type.Symbol
    case t => throw CompileError.undefinedError(t)
  }

  def lookupVar(table: SymbolTable, name: Name): Option[Var] = name match {
    case m: ModuleName => lookupModuleVar(table, m)
    case _ => {
      val message = s"${name} is invalid var name"
      throw CompileError.nameError(message)
    }
  }

  def addVar(table: SymbolTable, name: Name.ModuleName, expr: Node): SymbolTable = {
    val module = findModule(table, name.tree, name.paths)

    if (module.nonEmpty) {
      val newModule = Module.addVar(module.get, Var(name.name, expr))
      updateModule(table, newModule)
    } else {
      val pathString = name.paths.mkString("/")
      val message = s"Module ${pathString} in tree ${name.tree} is not valid"
      throw CompileError.moduleError(message)
    }
  }

  // TODO share Trait code with addVar
  // TODO test trait functionality
  def lookupTrait(table: SymbolTable, name: Name): Option[Trait] = name match {
    case m: ModuleName => lookupModuleTrait(table, m)
    case _ => {
      val message = s"${name} is invalid trait name"
      throw CompileError.nameError(message)
    }
  }

  def lookupTraitFnImpl(table: SymbolTable, state: State, name: Name, selfType: Type) = name match {
    case n: TraitFnName => lookupModuleTraitFnImpl(table, n, selfType)
    case _ => {
      val message = s"${name} is invalid trait fn name"
      throw CompileError.nameError(message)
    }
  }

  def addTrait(table: SymbolTable, name: Name.ModuleName, fnDefs: Map[String, FnDef]): SymbolTable = {
    val module = findModule(table, name.tree, name.paths)

    if (module.nonEmpty) {
      val newTrait = Trait(name.name, fnDefs)
      val newModule = Module.addTrait(module.get, newTrait)
      updateModule(table, newModule)
    } else {
      val pathString = name.paths.mkString("/")
      val message = s"Module ${pathString} in tree ${name.tree} is not valid"
      throw CompileError.moduleError(message)
    }
  }

  private def lookupQualifiedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val nameParts = raw.split('/').toList

    if (nameParts.size != 2) {
      throw CompileError.nameError(s"invalid qualified name format ${raw}")
    }
    val moduleParts :: rawName :: Nil = nameParts

    if (SymbolTable.isReservedName(rawName)) {
      throw CompileError.reservedWordError(rawName)
    }

    val treeName :: paths = moduleParts.split('.').toList
    val maybeTree = table.trees.get(treeName)

    val validModulePath = maybeTree.map(tree =>
      SymbolTable.isValidPath(tree, paths)).getOrElse(false)

    if (validModulePath) {
      // check if the path referenced is imported
      val mBase :: mPaths = paths.reverse
      val mName = ModuleName(treeName, mPaths, mBase)
      val varName = ModuleName(treeName, paths, rawName)
      val currentModule = getModule(table, state.currentModule)

      val isStdlibModule = mName == Common.module.name
      val isModuleImported = currentModule.imports.values.toList.contains(mName)

      if (!isStdlibModule && !isModuleImported) {
        val message = s"Var ${varName} is not imported in ${currentModule.name}"
        throw CompileError.moduleError(message)
      }

      // TODO this is wonky
      val moduleVar = lookupVar(table, varName).map(_ => varName)

      if (moduleVar.nonEmpty) {
        moduleVar
      } else {
        val traitTuple = findModule(table, treeName, paths).flatMap(maybeMod =>
          maybeMod.traits.find({ case (_, Trait(_, fndefs)) =>
            fndefs.mapValues(_.fnDef.fnName).contains(rawName)
          }))

        traitTuple.map(maybeTrait => {
          val traitName = maybeTrait._1
          val moduleName = ModuleName(treeName, paths, traitName)

          TraitFnName(moduleName, rawName)
        })
      }
    } else {
      val message = s"Module name ${raw} is not valid"
      throw CompileError.moduleError(message)
    }
  }

  private def lookupAliasedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val nameParts = raw.split('/').toList

    if (nameParts.size != 2) {
      throw CompileError.nameError(s"invalid qualified name format ${raw}")
    }

    val aliasName :: baseName :: Nil = nameParts

    if (SymbolTable.isReservedName(baseName)) {
      throw CompileError.reservedWordError(baseName)
    }

    val currentModule = getModule(table, state.currentModule)

    if (!currentModule.imports.contains(aliasName)) {
      throw CompileError.nameError(s"in ${raw}, no module alias ${aliasName}")
    }

    val fullName = currentModule.imports(aliasName)
    val varName = ModuleName.nest(fullName, baseName)
    val moduleVar = lookupVar(table, varName).map(_ => varName)

    if (moduleVar.nonEmpty) {
      moduleVar
    } else {
      val modulePaths = fullName.paths :+ fullName.name
      val maybeModule = findModule(table, fullName.tree, modulePaths)
      val traitTuple = maybeModule.flatMap(maybeMod =>
        maybeMod.traits.find({ case (_, Trait(_, fndefs)) =>
          fndefs.mapValues(_.fnDef.fnName).contains(baseName)
        }))

      traitTuple.map(maybeTrait => {
        val traitName = maybeTrait._1
        val moduleName = ModuleName.nest(fullName, traitName)

        TraitFnName(moduleName, baseName)
      })
    }
  }

  private def lookupUnqualifiedName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    if (SymbolTable.isReservedName(raw)) {
      throw CompileError.reservedWordError(raw)
    }

    val localBinding = state.localScopes.find(_.contains(raw))

    if (localBinding.nonEmpty) {
      Some(LocalName(raw))
    } else {
      val currentModule = getModule(table, state.currentModule)
      val name = currentModule.vars.get(raw)

      if (name.nonEmpty) {
        Some(Name.ModuleName.nest(state.currentModule, raw))
      } else {
        lookupGlobalName(table, state, raw)
      }
    }
  }

  private def lookupGlobalName(table: SymbolTable, state: State, raw: String): Option[Name] = {
    val flopTree = table.trees(Core.rootName)
    val commonModule = ModuleTree.findModule(flopTree, Common.path)

    val varName = commonModule.flatMap(maybeMod => maybeMod.vars.get(raw)).map(v =>
      ModuleName(Core.rootName, Common.path, v.name))

    if (varName.isEmpty) {
      commonModule.flatMap(maybeMod =>
        maybeMod.traits.values.find(t => t.fnDefs.contains(raw)).
        map(t => TraitFnName(ModuleName(Core.rootName, Common.path, t.name), raw)))
    } else {
      varName
    }
  }

  private def lookupModuleType(table: SymbolTable, name: ModuleName): Type = {
    val moduleType = lookupModuleVar(table, name).map(_.node.eType)

    if (moduleType.nonEmpty) {
      moduleType.get
    } else {
      throw CompileError.undefinedError(name.toString)
    }
  }

  private def lookupTraitFnType(table: SymbolTable, name: TraitFnName): Type = {
    val ModuleName(tree, paths, base) = name.moduleName

    val fnType = findModule(table, tree, paths).flatMap(maybeMod =>
      maybeMod.traits.get(base).flatMap(maybeTrait =>
        maybeTrait.fnDefs.get(name.name).map(maybeFnDef =>
          maybeFnDef.fnDef.eType)))

    if (fnType.nonEmpty) {
      fnType.get
    } else {
      throw CompileError.undefinedError(name.toString)
    }
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
      flatMap(maybeMod => Module.lookupVar(maybeMod, name.name))
  }

  private def lookupModuleTrait(table: SymbolTable, name: ModuleName): Option[Trait] = {
    findModule(table, name.tree, name.paths).
      flatMap(maybeMod => Module.lookupTrait(maybeMod, name.name))
  }

  private def lookupModuleTraitFnImpl(table: SymbolTable, name: TraitFnName, selfType: Type): Option[Node.FnN] = {
    val TraitFnName(moduleName, fnName) = name
    val traitImplName = TraitFn(moduleName.name, fnName, selfType)

    findModule(table, moduleName.tree, moduleName.paths).
      flatMap(maybeMod => Module.lookupTraitImpl(maybeMod, traitImplName))
  }
}
