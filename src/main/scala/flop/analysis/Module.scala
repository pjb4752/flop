package flop.analysis

case class Module(name: String, traits: Module.Traits, vars: Module.Vars)

object Module {

  case class Var(name: String, eType: Type)
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
