package flop.stdlib.core

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

import scala.collection.immutable.{List => SList}

object List {

  val rootName = "flopcore"
  val coreName = "core"

  val name = "list"
  val path = SList(coreName, name)
  val moduleName = Name.ModuleName(rootName, SList(coreName), name)

  val listGeneric = Type.Generic("A")
  val newName = Name.ModuleName(rootName, path, "new")
  val newType = Type.LuaFn(
    SList(Type.Pack(listGeneric)),
    Type.List(SList(listGeneric)))
  val newVar = ModuleTree.Module.Var(newName.name,
    Node.LuaPFn(newType, newName))

  val headName = Name.ModuleName(rootName, path, "head")
  val headType = Type.LuaFn(
    SList(Type.List(SList(listGeneric))),
    listGeneric)
  val headVar = ModuleTree.Module.Var(headName.name,
    Node.LuaPFn(headType, headName))

  val tailName = Name.ModuleName(rootName, path, "tail")
  val tailType = Type.LuaFn(
    SList(Type.List(SList(listGeneric))),
    Type.List(SList(listGeneric)))
  val tailVar = ModuleTree.Module.Var(tailName.name,
    Node.LuaPFn(tailType, tailName))

  /*
   * Definition of 'list' module
   */
  val module = ModuleTree.Module(
    moduleName,
    Map[String, Name.ModuleName](),
    Map[String, ModuleTree.Module.Trait](),
    Map[ModuleTree.Module.TraitFn, Node.FnN](),
    Map(
      newName.name -> newVar,
      headName.name -> headVar,
      tailName.name -> tailVar
    )
  )
}
