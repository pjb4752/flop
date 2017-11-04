package flop.stdlib.core

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

import scala.collection.immutable.{List => SList, Map => SMap}

object Vector {

  val rootName = "flopcore"
  val coreName = "core"

  val name = "vector"
  val path = SList(coreName, name)
  val moduleName = Name.ModuleName(rootName, SList(coreName), name)

  val elementGeneric = Type.Generic("A")
  val newName = Name.ModuleName(rootName, path, "new")
  val newType = Type.LuaFn(
    SList(Type.Pack(elementGeneric)),
    Type.Vector(SList(elementGeneric)))
  val newVar = ModuleTree.Module.Var(newName.name,
    Node.LuaPFn(newType, newName))

  val getName = Name.ModuleName(rootName, path, "get")
  val getType = Type.LuaFn(
    SList(Type.Vector(SList(elementGeneric)), Type.Number),
    elementGeneric)
  val getVar = ModuleTree.Module.Var(getName.name,
    Node.LuaPFn(getType, getName))

  /*
   * Definition of 'list' module
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
