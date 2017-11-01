package flop.stdlib

import flop.analysis.{ModuleTree, Name, Node, Type}
import flop.analysis.Node

object Core {

  import flop.stdlib.core._

  val rootName = "flopcore"
  val coreName = "core"

  val stdLibImports = Map(
    Common.name -> Common.moduleName,
    List.name -> List.moduleName
  )

  /*
   * Definition of 'core' module tree
   */
  val library = ModuleTree(rootName,
    Map(
      coreName -> ModuleTree.SubTree(
        coreName,
        Map(
          Common.name -> Common.module,
          List.name -> List.module
        )
      )
    )
  )
}
