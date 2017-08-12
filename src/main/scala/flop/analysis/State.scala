package flop.analysis

import flop.reading.Form

case class State(
  atTopLevel: Boolean,
  currentModule: String,
  analyzeFn: (ModuleTree, State) => (Form => Node),
  localScopes: List[State.Scope] = List[State.Scope]())

case object State {
  type Scope = Map[String, Type]
}
