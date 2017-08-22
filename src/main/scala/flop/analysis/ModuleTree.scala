package flop.analysis

sealed trait MNode

case class ModuleTree(name: String, children: Map[String, MNode])

case object ModuleTree {

  case class SubTree(name: String, children: Map[String, MNode]) extends MNode
  case class Module(name: String, traits: Module.Traits, vars: Module.Vars) extends MNode

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
    assert(paths.nonEmpty)

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

    isValid(tree.children.get(paths.head), paths.tail)
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
}
