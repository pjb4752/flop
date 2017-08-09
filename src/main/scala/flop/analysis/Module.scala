package flop.analysis

case class Module(vars: Map[String, Type],
    traits: Map[String, Map[String, Node.FnDef]]) {

  def addVar(name: String, vType: Type) =
    this.copy(vars = vars + (name -> vType))

  def addTrait(name: String, fnDefs: Map[String, Node.FnDef]) =
    this.copy(traits = traits + (name -> fnDefs))
}
