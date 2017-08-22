package flop.analysis

import flop.reading.Form

case class State(
  analyzeFn: (ModuleTree, State) => (Form => Node),
  atTopLevel: Boolean,
  currentTree: String,
  currentPaths: List[String] = List[String](),
  localScopes: List[State.Scope] = List[State.Scope]())

// TODO String should be changed to LocalName
case object State {
  type Scope = Map[String, Type]
}
