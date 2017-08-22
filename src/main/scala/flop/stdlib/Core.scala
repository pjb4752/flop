package flop.stdlib

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

object Core {

  val coreName = "core"
  val commonName = "common"
  val commonPath = List(commonName)

  /*
   * Definition of 'common' trait Show which allows string conversions
   * Function Defs:
   * str - takes 'self' and returns 'self' as string
   */
  val showName = Name.ModuleName(coreName, commonPath, "Show")
  val strName = Name.ModuleName(coreName, commonPath, "str")
  val showStrType = Type.TraitFn(List(Type.Self), Type.String)
  val showTrait = ModuleTree.Module.Trait(showName.name,
    Map(strName.name ->
      ModuleTree.Module.FnDef(
        strName.name,
        Node.FnDef(
          Node.SymLit(showName, Type.Trait),
          Node.SymLit(strName, showStrType),
          showStrType
        )
      )
    )
  )

  /*
   * Definition of 'common' vars
   */
  val plusName = Name.ModuleName(coreName, commonPath, "+")
  val plusType = Type.LuaFn(List(Type.Number, Type.Number), Type.Number)
  val plusVar = ModuleTree.Module.Var(plusName.name, Node.LuaIFn(plusType, "+"))

  /*
   * Definition of 'common' module
   */
  val commonModule = ModuleTree.Module(commonName,
    Map(
      showName.name -> showTrait
    ),
    Map(
      plusName.name -> plusVar
    )
  )

  /*
   * Definition of 'core' module tree
   */
  val library = ModuleTree(coreName,
    Map(
      commonName -> commonModule
    )
  )

  //val builtins = Map[String, Node](
    //"true" -> Node.TrueLit,
    //"false" -> Node.FalseLit,
    //"+" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "+"),
    //"-" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "-"),
    //"*" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "*"),
    //"/" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "/"),
    //">" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), ">"),
    //"<" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "<"),
    //"=" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "=="),
    //">=" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), ">="),
    //"<=" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "<="),
    //"<>" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "~="),
    //"print" -> Node.LuaPFn(Type.LuaFn(List(Type.String), Type.Unit), "print"),
    //"num_to_s" -> Node.LuaPFn(Type.TraitFn(List(Type.Number), Type.String), "num_to_s")
  //)

  //val showStrType = Type.TraitFn(List(Type.Self), Type.String)

  //val builtinTraits = Map[String, List[Node.FnDef]](
    //"show" -> List(
      //Node.FnDef(
        //Node.SymLit("show", Type.Trait),
        //Node.SymLit("str", showStrType),
        //showStrType
      //)
    //)
  //)

  //val numShowImpl = Node.TraitImpl(
      //Type.Number,
      //Node.SymLit("show", Type.Trait),
      //Map[Node.SymLit, Node.FnN](
        //Node.SymLit("str", showStrType) ->
        //Node.LuaPFn(showStrType, "num_to_s")
      //)
  //)

  //val boolShowImpl = Node.TraitImpl(
    //Type.Boolean,
    //Node.SymLit("show", Type.Trait),
    //Map[Node.SymLit, Node.FnN](
      //Node.SymLit("str", showStrType) ->
      //Node.LuaPFn(showStrType, "bool_to_s")
    //)
  //)

  //val traitImpls = Map[String, Map[Type, Node.TraitImpl]](
    //"str" -> Map(
      //Type.Number -> numShowImpl,
      //Type.Boolean -> boolShowImpl
    //)
  //)
}
