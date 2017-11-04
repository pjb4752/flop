package flop.stdlib

import scala.collection.immutable.{Map => SMap}

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

object Core {

  import flop.stdlib.core._

  val rootName = "flopcore"
  val coreName = "core"

  val stdLibImports = SMap(
    Common.name -> Common.moduleName,
    List.name -> List.moduleName,
    Map.name -> Map.moduleName,
    Pair.name -> Pair.moduleName,
    Vector.name -> Vector.moduleName
  )

  /*
   * Definition of 'core' module tree
   */
  val library = ModuleTree(rootName,
    SMap(
      coreName -> ModuleTree.SubTree(
        coreName,
        SMap(
          Common.name -> Common.module,
          List.name -> List.module,
          Map.name -> Map.module,
          Pair.name -> Pair.module,
          Vector.name -> Vector.module
        )
      )
    )
  )
}
