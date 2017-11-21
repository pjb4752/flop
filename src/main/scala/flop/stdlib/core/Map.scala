package flop.stdlib.core

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

import scala.collection.immutable.{List => SList, Map => SMap}

object Map {

  val rootName = "flopcore"
  val coreName = "core"

  val name = "map"
  val path = SList(coreName, name)
  val moduleName = Name.ModuleName(rootName, SList(coreName), name)

  val generic1 = Type.Generic("A")
  val generic2 = Type.Generic("B")
  val newName = Name.ModuleName(rootName, path, "new")
  val newType = Type.LuaFn(
    SList(Type.Pack(Type.Pair(SList(generic1, generic2)))),
    Type.Map(SList(generic1, generic2)))
  val newVar = ModuleTree.Module.Var(newName.name,
    Node.LuaPFn(newType, newName))
  val getName = Name.ModuleName(rootName, path, "get")
  val getType = Type.LuaFn(
    SList(Type.Map(SList(generic1, generic2)), generic1),
    generic2)
  val getVar = ModuleTree.Module.Var(getName.name,
    Node.LuaPFn(getType, getName))

  /*
   * Definition of 'map' module
   */
  val module = ModuleTree.Module(
    moduleName,
    SMap[String, Name.ModuleName](),
    SMap[String, ModuleTree.Module.Trait](),
    SMap[ModuleTree.Module.TraitFn, Node.FnN](),
    SMap(
      newName.name -> newVar,
      getName.name -> getVar
    )
  )
}
