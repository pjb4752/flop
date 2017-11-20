package flop.stdlib.core

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

import scala.collection.immutable.{List => SList, Map => SMap}

object Common {

  val rootName = "flopcore"
  val coreName = "core"

  val name = "common"
  val path = SList(coreName, name)
  val moduleName = Name.ModuleName(rootName, SList(coreName), name)

  /*
   * Definition of 'common' trait Show which allows string conversions
   * Function Defs:
   * str - takes param of type Self and returns its string representation
   */
  val showName = Name.ModuleName(rootName, path, "Show")
  val strName = Name.TraitFnName(showName, "str")
  val strType = Type.TraitFn(SList(Type.Self), Type.String)
  val showTrait = ModuleTree.Module.Trait(showName.name,
    SMap(
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
  val equalityName = Name.ModuleName(rootName, path, "Equality")
  val eqName = Name.TraitFnName(equalityName, "=")
  val neqName = Name.TraitFnName(equalityName, "not=")
  val eqType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Boolean)
  val neqType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Boolean)
  val equalityTrait = ModuleTree.Module.Trait(equalityName.name,
    SMap(
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

  val orderingName = Name.ModuleName(rootName, path, "Ordering")
  val ltName = Name.TraitFnName(orderingName, "<")
  val gtName = Name.TraitFnName(orderingName, ">")
  val lteName = Name.TraitFnName(orderingName, "<=")
  val gteName = Name.TraitFnName(orderingName, ">=")
  val ltType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Boolean)
  val gtType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Boolean)
  val lteType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Boolean)
  val gteType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Boolean)
  val orderingTrait = ModuleTree.Module.Trait(orderingName.name,
    SMap(
      ltName.name -> ModuleTree.Module.FnDef(
        ltName.name,
        Node.FnDef(
          Node.SymLit(orderingName, Type.Trait),
          Node.SymLit(ltName, ltType),
          ltType
        )
      ),
      gtName.name -> ModuleTree.Module.FnDef(
        gtName.name,
        Node.FnDef(
          Node.SymLit(orderingName, Type.Trait),
          Node.SymLit(gtName, gtType),
          gtType
        )
      ),
      lteName.name -> ModuleTree.Module.FnDef(
        lteName.name,
        Node.FnDef(
          Node.SymLit(orderingName, Type.Trait),
          Node.SymLit(lteName, lteType),
          lteType
        )
      ),
      gteName.name -> ModuleTree.Module.FnDef(
        gteName.name,
        Node.FnDef(
          Node.SymLit(orderingName, Type.Trait),
          Node.SymLit(gteName, gteType),
          gteType
        )
      ),
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
  val numericName = Name.ModuleName(rootName, path, "Numeric")
  val plusName = Name.TraitFnName(numericName, "+")
  val minusName = Name.TraitFnName(numericName, "-")
  val multName = Name.TraitFnName(numericName, "*")
  val divName = Name.TraitFnName(numericName, "/")
  val plusType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Self)
  val minusType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Self)
  val multType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Self)
  val divType = Type.TraitFn(SList(Type.Self, Type.Self), Type.Self)
  val numericTrait = ModuleTree.Module.Trait(numericName.name,
    SMap(
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
  val showTraitImpl = SMap(
    ModuleTree.Module.TraitFn(showName.name, strName.name, Type.Number) ->
      Node.LuaPFn(strType, Name.TraitFnName(showName, "num_to_str")),
    ModuleTree.Module.TraitFn(showName.name, strName.name, Type.Boolean) ->
      Node.LuaPFn(strType, Name.TraitFnName(showName, "bool_to_str"))
    )

  val equalityTraitImpl = SMap(
    ModuleTree.Module.TraitFn(equalityName.name, eqName.name, Type.Number) ->
      Node.LuaIFn(eqType, Name.LocalName("==")),
    ModuleTree.Module.TraitFn(equalityName.name, neqName.name, Type.Number) ->
      Node.LuaIFn(neqType, Name.LocalName("~=")),
    ModuleTree.Module.TraitFn(equalityName.name, eqName.name, Type.Boolean) ->
      Node.LuaIFn(eqType, Name.LocalName("==")),
    ModuleTree.Module.TraitFn(equalityName.name, neqName.name, Type.Boolean) ->
      Node.LuaIFn(neqType, Name.LocalName("~=")),
    ModuleTree.Module.TraitFn(equalityName.name, eqName.name, Type.String) ->
      Node.LuaIFn(eqType, Name.LocalName("==")),
    ModuleTree.Module.TraitFn(equalityName.name, neqName.name, Type.String) ->
      Node.LuaIFn(neqType, Name.LocalName("~="))
    )

  val orderingTraitImpl = SMap(
    ModuleTree.Module.TraitFn(orderingName.name, ltName.name, Type.Number) ->
      Node.LuaIFn(ltType, Name.LocalName("<")),
    ModuleTree.Module.TraitFn(orderingName.name, gtName.name, Type.Number) ->
      Node.LuaIFn(gtType, Name.LocalName(">")),
    ModuleTree.Module.TraitFn(orderingName.name, lteName.name, Type.Number) ->
      Node.LuaIFn(lteType, Name.LocalName("<=")),
    ModuleTree.Module.TraitFn(orderingName.name, gteName.name, Type.Number) ->
      Node.LuaIFn(gteType, Name.LocalName(">="))
  )

  val numericTraitImpl = SMap(
    ModuleTree.Module.TraitFn(numericName.name, plusName.name, Type.Number) ->
      Node.LuaIFn(plusType, Name.LocalName("+")),
    ModuleTree.Module.TraitFn(numericName.name, minusName.name, Type.Number) ->
      Node.LuaIFn(minusType, Name.LocalName("-")),
    ModuleTree.Module.TraitFn(numericName.name, multName.name, Type.Number) ->
      Node.LuaIFn(multType, Name.LocalName("*")),
    ModuleTree.Module.TraitFn(numericName.name, divName.name, Type.Number) ->
      Node.LuaIFn(divType, Name.LocalName("/")),
  )

  val commonTraitImpls =
    showTraitImpl ++
    equalityTraitImpl ++
    orderingTraitImpl ++
    numericTraitImpl

  /*
   * IO functions TODO make this is a trait?
   */
  val printName = Name.ModuleName(rootName, path, "print")
  val printType = Type.LuaFn(SList(Type.String), Type.Unit)
  val printVar = ModuleTree.Module.Var(printName.name,
    Node.LuaPFn(printType, Name.LocalName("print")))

  /*
   * Definition of 'common' module
   */
  val module = ModuleTree.Module(
    moduleName,
    SMap[String, Name.ModuleName](),
    SMap(
      showName.name -> showTrait,
      equalityName.name -> equalityTrait,
      orderingName.name -> orderingTrait,
      numericName.name -> numericTrait
    ),
    commonTraitImpls,
    SMap(
      printName.name -> printVar
    )
  )
}
