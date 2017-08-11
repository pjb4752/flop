package flop.analysis

case class State(
  atModuleLevel: Boolean,
  currentModule: Module,
  modules: State.Modules,
  scopes: List[State.Scope] = List[State.Scope]())

object State {

  type Modules = Map[String, Module]
  type Scope = Map[String, Type]

  def initial: State = {
    // default module is 'user' module
    val userModule = Module.initial("user")
    val initialModules = Map(userModule.name -> userModule)

    State(true, userModule, initialModules, List[Scope]())
  }

  def switchModule(state: State, name: String): State = {
    val module = state.modules.getOrElse(name, Module.initial(name))
    updateModule(state, module)
  }

  def addScope(state: State, scope: Scope): State = {
    state.copy(scopes = scope :: state.scopes)
  }

  def addTrait(state: State, newTrait: Module.Trait): State = {
    val updatedModule = Module.addTrait(state.currentModule, newTrait)
    updateModule(state, updatedModule)
  }

  def addTrait(state: State, traitName: String, traitDefs: Module.FnDefs): State = {
    addTrait(state, Module.Trait(traitName, traitDefs))
  }

  def addVar(state: State, newVar: Module.Var): State = {
    val updatedModule = Module.addVar(state.currentModule, newVar)
    updateModule(state, updatedModule)
  }

  def addVar(state: State, varName: String, varType: Type): State = {
    addVar(state, Module.Var(varName, varType))
  }

  private def updateModule(state: State, module: Module): State = {
    val newModules = state.modules + (module.name -> module)
    state.copy(modules = newModules, currentModule = module)
  }
}
