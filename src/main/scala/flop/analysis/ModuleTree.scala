package flop.analysis

sealed trait MNode

case class ModuleTree(name: String, children: Map[String, MNode])

case object ModuleTree {

  case class SubTree(
    name: String,
    children: Map[String, MNode] = Map[String, MNode]()
  ) extends MNode

  case class Module(
    name: Name.ModuleName,
    imports: Map[String, Name.ModuleName],
    traits: Module.Traits = Map[String, Module.Trait](),
    traitImpls: Module.TraitImpls = Map[Module.TraitFn, Node.FnN](),
    vars: Module.Vars = Map[String, Module.Var]()
  ) extends MNode

  object Module {
    case class Var(name: String, node: Node)
    type Vars = Map[String, Var]

    case class FnDef(name: String, fnDef: Node.FnDef)
    type FnDefs = Map[String, FnDef]

    case class Trait(name: String, fnDefs: FnDefs)
    type Traits = Map[String, Trait]

    case class TraitFn(traitName: String, fnName: String, selfType: Type)
    type TraitImpls = Map[TraitFn, Node.FnN]

    def initial(name: Name.ModuleName,
        initialImports: Map[String, Name.ModuleName]): Module = {
      val initialTraits = Map[String, Trait]()
      val initialTraitImpls = Map[TraitFn, Node.FnN]()
      val initialVars = Map[String, Var]()

      Module(name, initialImports, initialTraits, initialTraitImpls, initialVars)
    }

    def lookupVar(module: Module, varName: String): Option[Var] = {
      module.vars.get(varName)
    }

    def addVar(module: Module, newVar: Var): Module = {
      module.copy(vars = module.vars + (newVar.name -> newVar))
    }

    def lookupTrait(module: Module, traitName: String): Option[Trait] = {
      module.traits.get(traitName)
    }

    def lookupTraitImpl(module: Module, traitFn: TraitFn): Option[Node.FnN] = {
      module.traitImpls.get(traitFn)
    }

    def addTrait(module: Module, newTrait: Trait): Module = {
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

  def addModule(tree: ModuleTree, module: Module): ModuleTree = {
    assert(module.name.paths.nonEmpty)

    def add(node: MNode, paths: List[String], module: Module): SubTree = {
      node match {
        case m: Module => {
          val message = s"Module path is invalid: ${paths}"
          throw CompileError.moduleError(message)
        }
        case t: SubTree => {
          if (paths.isEmpty && t.children.contains(module.name.name)) {
            val message = s"Module ${module.name} is already defined"
            throw CompileError.moduleError(message)
          } else if (paths.isEmpty) {
            t.copy(children = t.children + (module.name.name -> module))
          } else {
            val blankTree = SubTree(paths.head)
            val childTree = t.children.getOrElse(paths.head, blankTree)
            val newSubTree = add(childTree, paths.tail, module)

            t.copy(children = t.children + (newSubTree.name -> newSubTree))
          }
        }
      }
    }

    val paths = module.name.paths
    val blankTree = SubTree(paths.head)
    val subTree = tree.children.getOrElse(paths.head, blankTree)
    val newSubTree = add(subTree, paths.tail, module)

    tree.copy(children = tree.children + (newSubTree.name -> newSubTree))
  }

  def updateModule(tree: ModuleTree, module: Module): ModuleTree = {
    assert(module.name.paths.nonEmpty)

    def update(node: MNode, paths: List[String], module: Module): SubTree = {
      node match {
        case m: Module => {
          val message = s"Module path is invalid: ${paths}"
          throw CompileError.moduleError(message)
        }
        case t: SubTree => {
          if (paths.isEmpty) {
            if (t.children.contains(module.name.name)) {
              t.copy(children = t.children + (module.name.name -> module))
            } else {
              val message = s"Module ${module.name} is not defined"
              throw CompileError.moduleError(message)
            }
          } else {
            if (t.children.contains(paths.head)) {
              val nextNode = t.children(paths.head)
              update(nextNode, paths.tail, module)
            } else {
              val message = s"Module ${module.name} is not defined"
              throw CompileError.moduleError(message)
            }
          }
        }
      }
    }

    val paths = module.name.paths
    val subTree = tree.children.get(paths.head)

    if (subTree.nonEmpty) {
      val newSubTree = update(subTree.get, paths.tail, module)
      tree.copy(children = tree.children + (newSubTree.name -> newSubTree))
    } else {
      val message = s"ModuleTree is invalid: ${paths.head}"
      throw CompileError.moduleError(message)
    }
  }

}
