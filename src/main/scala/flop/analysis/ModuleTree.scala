package flop.analysis

sealed trait MNode

case class SubModule(name: String, value: Module) extends MNode
case class ModuleTree(name: String, children: Map[String, MNode]) extends MNode

case object ModuleTree {

  def newRoot(name: String) = ModuleTree(name, Map[String, MNode]())

  def addChildModule(tree: ModuleTree, module: Module): ModuleTree = {
    val newNode = SubModule(module.name, module)
    tree.copy(children = tree.children + (module.name -> newNode))
  }

  def findModule(tree: ModuleTree, paths: List[String]): Option[Module] = {
    // TODO this can stack overflow
    // @scala.annotation.tailrec
    def findModule0(tree: ModuleTree, paths: List[String]): Option[Module] = {
      tree.children.get(paths.head).flatMap(node => node match {
        case m: SubModule => if (paths.isEmpty) Some(m.value) else None
        case t: ModuleTree => findModule0(t, paths.tail)
      })
    }

    assert(paths.nonEmpty)
    if (tree.name == paths.head) None
    else findModule(tree, paths)
  }

  def validatePath(tree: ModuleTree, paths: List[String]): Boolean = {
    assert(paths.nonEmpty)
    return tree.name == paths.head && validateTree(tree, paths.tail)
  }

  @scala.annotation.tailrec
  private def validateTree(tree: ModuleTree, paths: List[String]): Boolean = {
    if (paths.isEmpty) true
    else if (!tree.children.contains(paths.head)) false
    else {
      tree.children(paths.head) match {
        case t: ModuleTree => validateTree(t, paths.tail)
        case _ => paths.tail.isEmpty
      }
    }
  }
}
