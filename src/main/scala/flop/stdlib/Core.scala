package flop.stdlib

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

object Core {

  val rootName = "flop"
  val coreName = "core"
  val commonName = "common"
  val commonPath = List(coreName, commonName)

  /*
   * Definition of 'common' trait Show which allows string conversions
   * Function Defs:
   * str - takes 'self' and returns 'self' as string
   */
  val showName = Name.ModuleName(rootName, commonPath, "Show")
  val strName = Name.ModuleName(rootName, commonPath, "str")
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

  val equalityName = Name.ModuleName(rootName, commonPath, "Equality")
  val eqName = Name.ModuleName(rootName, commonPath, "=")
  val neqName = Name.ModuleName(rootName, commonPath, "<>")
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

  val traitImpls =
    showTraitImpl ++
    equalityTraitImpl

  /*
   * Math functions
   */
  val plusName = Name.ModuleName(rootName, commonPath, "+")
  val plusType = Type.LuaFn(List(Type.Number, Type.Number), Type.Number)
  val plusVar = ModuleTree.Module.Var(plusName.name, Node.LuaIFn(plusType, "+"))

  val minusName = Name.ModuleName(rootName, commonPath, "-")
  val minusType = Type.LuaFn(List(Type.Number, Type.Number), Type.Number)
  val minusVar = ModuleTree.Module.Var(minusName.name, Node.LuaIFn(minusType, "-"))

  val multName = Name.ModuleName(rootName, commonPath, "*")
  val multType = Type.LuaFn(List(Type.Number, Type.Number), Type.Number)
  val multVar = ModuleTree.Module.Var(multName.name, Node.LuaIFn(multType, "*"))

  val divName = Name.ModuleName(rootName, commonPath, "/")
  val divType = Type.LuaFn(List(Type.Number, Type.Number), Type.Number)
  val divVar = ModuleTree.Module.Var(divName.name, Node.LuaIFn(divType, "/"))

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
      equalityName.name -> equalityTrait
    ),
    traitImpls,
    Map(
      plusName.name -> plusVar,
      minusName.name -> minusVar,
      multName.name -> multVar,
      divName.name -> divVar,
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
