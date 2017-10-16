package flop.stdlib

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

object Core {

  val rootName = "flopcore"
  val coreName = "core"
  val commonName = "common"
  val commonPath = List(coreName, commonName)

  /*
   * Definition of 'common' trait Show which allows string conversions
   * Function Defs:
   * str - takes param of type Self and returns its string representation
   */
  val showName = Name.ModuleName(rootName, commonPath, "Show")
  val strName = Name.TraitFnName(showName, "str")
  val strType = Type.TraitFn(List(Type.Self), Type.String)
  val showTrait = ModuleTree.Module.Trait(showName.name,
    Map(
      strName.name -> ModuleTree.Module.FnDef(
        strName.name,
        Node.FnDef(
          Node.SymLit(showName, Type.Trait),
          Node.SymLit(strName, strType),
          strType
        )
      )
    )
  )

  /*
   * Definition of 'common' trait Equality which allows equality comparisons
   * Function Defs:
   * = - takes two params of Self type and tests them for equality
   * <> - takes two params of Self type and tests them for inequality
   */
  val equalityName = Name.ModuleName(rootName, commonPath, "Equality")
  val eqName = Name.TraitFnName(equalityName, "=")
  val neqName = Name.TraitFnName(equalityName, "not=")
  val eqType = Type.TraitFn(List(Type.Self, Type.Self), Type.Boolean)
  val neqType = Type.TraitFn(List(Type.Self, Type.Self), Type.Boolean)
  val equalityTrait = ModuleTree.Module.Trait(equalityName.name,
    Map(
      eqName.name -> ModuleTree.Module.FnDef(
        eqName.name,
        Node.FnDef(
          Node.SymLit(equalityName, Type.Trait),
          Node.SymLit(eqName, eqType),
          eqType
        )
      ),
      neqName.name -> ModuleTree.Module.FnDef(
        neqName.name,
        Node.FnDef(
          Node.SymLit(equalityName, Type.Trait),
          Node.SymLit(neqName, neqType),
          neqType
        )
      )
    )
  )

  /*
   * Definition of 'common' trait Numeric which allows numerical comparisons
   * Function Defs:
   * + - takes two params of Self type and performs addition
   * - - takes two params of Self type and performs subtraction
   * * - takes two params of Self type and performs multiplication
   * / - takes two params of Self type and performs division
   */
  val numericName = Name.ModuleName(rootName, commonPath, "Numeric")
  val plusName = Name.TraitFnName(numericName, "+")
  val minusName = Name.TraitFnName(numericName, "-")
  val multName = Name.TraitFnName(numericName, "*")
  val divName = Name.TraitFnName(numericName, "/")
  val plusType = Type.TraitFn(List(Type.Self, Type.Self), Type.Self)
  val minusType = Type.TraitFn(List(Type.Self, Type.Self), Type.Self)
  val multType = Type.TraitFn(List(Type.Self, Type.Self), Type.Self)
  val divType = Type.TraitFn(List(Type.Self, Type.Self), Type.Self)
  val numericTrait = ModuleTree.Module.Trait(numericName.name,
    Map(
      plusName.name -> ModuleTree.Module.FnDef(
        plusName.name,
        Node.FnDef(
          Node.SymLit(numericName, Type.Trait),
          Node.SymLit(plusName, plusType),
          plusType
        )
      ),
      minusName.name -> ModuleTree.Module.FnDef(
        minusName.name,
        Node.FnDef(
          Node.SymLit(numericName, Type.Trait),
          Node.SymLit(minusName, minusType),
          minusType
        )
      ),
      multName.name -> ModuleTree.Module.FnDef(
        multName.name,
        Node.FnDef(
          Node.SymLit(numericName, Type.Trait),
          Node.SymLit(multName, multType),
          multType
        )
      ),
      divName.name -> ModuleTree.Module.FnDef(
        divName.name,
        Node.FnDef(
          Node.SymLit(numericName, Type.Trait),
          Node.SymLit(divName, divType),
          divType
        )
      )
    )
  )

  /*
   * Implementation of 'common' traits for builtin types
   */
  val showTraitImpl = Map(
    ModuleTree.Module.TraitFn(showName.name, strName.name, Type.Number) ->
      Node.LuaPFn(strType, "num_to_str"),
    ModuleTree.Module.TraitFn(showName.name, strName.name, Type.Boolean) ->
      Node.LuaPFn(strType, "bool_to_str")
    )

  val equalityTraitImpl = Map(
    ModuleTree.Module.TraitFn(equalityName.name, eqName.name, Type.Number) ->
      Node.LuaIFn(eqType, "=="),
    ModuleTree.Module.TraitFn(equalityName.name, neqName.name, Type.Number) ->
      Node.LuaIFn(neqType, "~="),
    ModuleTree.Module.TraitFn(equalityName.name, eqName.name, Type.Boolean) ->
      Node.LuaIFn(eqType, "=="),
    ModuleTree.Module.TraitFn(equalityName.name, neqName.name, Type.Boolean) ->
      Node.LuaIFn(neqType, "~="),
    ModuleTree.Module.TraitFn(equalityName.name, eqName.name, Type.String) ->
      Node.LuaIFn(eqType, "=="),
    ModuleTree.Module.TraitFn(equalityName.name, neqName.name, Type.String) ->
      Node.LuaIFn(neqType, "~=")
    )

  val numericTraitImpl = Map(
    ModuleTree.Module.TraitFn(numericName.name, plusName.name, Type.Number) ->
      Node.LuaIFn(plusType, "+"),
    ModuleTree.Module.TraitFn(numericName.name, minusName.name, Type.Number) ->
      Node.LuaIFn(minusType, "-"),
    ModuleTree.Module.TraitFn(numericName.name, multName.name, Type.Number) ->
      Node.LuaIFn(multType, "*"),
    ModuleTree.Module.TraitFn(numericName.name, divName.name, Type.Number) ->
      Node.LuaIFn(divType, "/"),
  )

  val traitImpls =
    showTraitImpl ++
    equalityTraitImpl ++
    numericTraitImpl

  /*
   * Comparison functions TODO make this a trait
   */
  val gtName = Name.ModuleName(rootName, commonPath, ">")
  val gtType = Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean)
  val gtVar = ModuleTree.Module.Var(gtName.name, Node.LuaIFn(gtType, ">"))

  val gteName = Name.ModuleName(rootName, commonPath, ">=")
  val gteType = Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean)
  val gteVar = ModuleTree.Module.Var(gteName.name, Node.LuaIFn(gteType, ">="))

  val ltName = Name.ModuleName(rootName, commonPath, "<")
  val ltType = Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean)
  val ltVar = ModuleTree.Module.Var(ltName.name, Node.LuaIFn(ltType, "<"))

  val lteName = Name.ModuleName(rootName, commonPath, "<=")
  val lteType = Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean)
  val lteVar = ModuleTree.Module.Var(lteName.name, Node.LuaIFn(lteType, "<="))

  /*
   * IO functions TODO make this is a trait?
   */
  val printName = Name.ModuleName(rootName, commonPath, "print")
  val printType = Type.LuaFn(List(Type.String), Type.Unit)
  val printVar = ModuleTree.Module.Var(printName.name, Node.LuaPFn(printType, "print"))

  /*
   * Definition of 'common' module
   */
  val commonModule = ModuleTree.Module(
    Name.ModuleName(rootName, List[String](coreName), commonName),
    Map[String, Name.ModuleName](),
    Map(
      showName.name -> showTrait,
      equalityName.name -> equalityTrait,
      numericName.name -> numericTrait
    ),
    traitImpls,
    Map(
      gtName.name -> gtVar,
      gteName.name -> gteVar,
      ltName.name -> ltVar,
      lteName.name -> lteVar,
      printName.name -> printVar
    )
  )

  /*
   * Definition of 'core' module tree
   */
  val library = ModuleTree(rootName,
    Map(
      coreName -> ModuleTree.SubTree(
        commonName,
        Map(
          commonName -> commonModule
        )
      )
    )
  )
}
