package flop.analysis

sealed trait MNode

case class ModuleTree(name: String, children: Map[String, MNode])

case object ModuleTree {

  case class SubTree(
    name: String,
    children: Map[String, MNode] = Map[String, MNode]()
  ) extends MNode

  case class Module(
    name: String,
    traits: Module.Traits = Map[String, Module.Trait](),
    vars: Module.Vars = Map[String, Module.Var]()
  ) extends MNode

  case class InvalidPathError(path: List[String]) extends flop.analysis.Error {
    val message = s"""invalid module path: ${path.mkString(".")}"""
  }

  case class ModuleExistsError(name: String) extends flop.analysis.Error {
    val message = s"module named ${name} already defined"
  }

  object Module {
    case class Var(name: String, node: Node)
    type Vars = Map[String, Var]

    case class FnDef(name: String, fnDef: Node.FnDef)
    type FnDefs = Map[String, FnDef]

    case class Trait(name: String, fnDefs: FnDefs)
    type Traits = Map[String, Trait]

    def initial(name: String): Module = {
      Module(name, Map[String, Trait](), Map[String, Var]())
    }

    def addVar(module: Module, newVar: Var) = {
      module.copy(vars = module.vars + (newVar.name -> newVar))
    }

    def addTrait(module: Module, newTrait: Trait) = {
      module.copy(traits = module.traits + (newTrait.name -> newTrait))
    }
  }

  def newRoot(name: String) = ModuleTree(name, Map[String, MNode]())

  def isValidPath(tree: ModuleTree, paths: List[String]): Boolean = {
    //TODO @scala.annotation.tailrec
    def isValid(node: Option[MNode], paths: List[String]): Boolean = {
      node.map(maybeNode =>
        maybeNode match {
          case m: Module => paths.isEmpty
          case t: SubTree if paths.isEmpty => false
          case t: SubTree => isValid(t.children.get(paths.head), paths.tail)
        }
      ).getOrElse(false)
    }

    paths.nonEmpty && isValid(tree.children.get(paths.head), paths.tail)
  }

  def findModule(tree: ModuleTree, paths: List[String]): Option[Module] = {
    assert(paths.nonEmpty)

    //TODO @scala.annotation.tailrec
    def find(node: Option[MNode], paths: List[String]): Option[Module] = {
      node.flatMap(maybeNode =>
        maybeNode match {
          case m: Module if paths.isEmpty => Some(m)
          case _: Module => None
          case _: SubTree if paths.isEmpty => None
          case t: SubTree => find(t.children.get(paths.head), paths.tail)
        }
      )
    }

    find(tree.children.get(paths.head), paths.tail)
  }

  def addModule(tree: ModuleTree, paths: List[String], module: Module): ModuleTree = {
    assert(paths.nonEmpty)

    def add(node: MNode, paths: List[String], module: Module): SubTree = {
      node match {
        case m: Module => throw CompileError(InvalidPathError(paths))
        case t: SubTree => {
          if (paths.isEmpty && t.children.contains(module.name)) {
            throw CompileError(ModuleExistsError(module.name))
          } else if (paths.isEmpty) {
            t.copy(children = t.children + (module.name -> module))
          } else {
            val blankTree = SubTree(paths.head)
            val childTree = t.children.getOrElse(paths.head, blankTree)
            val newSubTree = add(childTree, paths.tail, module)

            t.copy(children = t.children + (newSubTree.name -> newSubTree))
          }
        }
      }
    }

    val blankTree = SubTree(paths.head)
    val subTree = tree.children.getOrElse(paths.head, blankTree)
    val newSubTree = add(subTree, paths.tail, module)

    tree.copy(children = tree.children + (newSubTree.name -> newSubTree))
  }
}
