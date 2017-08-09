package flop.analysis

case class State(isModuleLevel: Boolean, moduleTraits: Map[String, List[Node.FnDef]],
    moduleVars: Map[String, Type], localVars: List[Map[String, Type]]) {

  def insertModuleTrait(name: String, fnDefs: List[Node.FnDef]) = {
    this.copy(moduleTraits = moduleTraits + (name -> fnDefs))
  }

  def insertModuleVar(name: String, vType: Type): State = {
    this.copy(moduleVars = moduleVars + (name -> vType))
  }

  def insertLocalScope(newVars: Map[String, Type]): State = {
    this.copy(localVars = newVars :: localVars)
  }
}

object State {

  def initial: State = {
    State(true,
      Map[String, List[Node.FnDef]](),
      Map[String, Type](),
      List[Map[String, Type]]())
  }
}
