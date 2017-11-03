package flop.stdlib.core

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

import scala.collection.immutable.{List => SList}

object Pair {

  val rootName = "flopcore"
  val coreName = "core"

  val name = "pair"
  val path = SList(coreName, name)
  val moduleName = Name.ModuleName(rootName, SList(coreName), name)

  val generic1 = Type.Generic("A")
  val generic2 = Type.Generic("B")
  val newName = Name.ModuleName(rootName, path, "new")
  val newType = Type.LuaFn(
    SList(generic1, generic2),
    Type.Pair(SList(generic1, generic2)))
  val newVar = ModuleTree.Module.Var(newName.name,
    Node.LuaPFn(newType, newName))

  val firstName = Name.ModuleName(rootName, path, "first")
  val firstType = Type.LuaFn(
    SList(Type.Pair(SList(generic1, generic2))),
    generic1)
  val firstVar = ModuleTree.Module.Var(firstName.name,
    Node.LuaPFn(firstType, firstName))

  val lastName = Name.ModuleName(rootName, path, "last")
  val lastType = Type.LuaFn(
    SList(Type.Pair(SList(generic1, generic2))),
    generic2)
  val lastVar = ModuleTree.Module.Var(lastName.name,
    Node.LuaPFn(lastType, lastName))

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
      firstName.name -> firstVar,
      lastName.name -> lastVar
    )
  )
}
