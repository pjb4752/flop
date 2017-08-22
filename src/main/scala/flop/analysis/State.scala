package flop.analysis

import flop.reading.Form

case class State(
  analyzeFn: (SymbolTable, State) => (Form => Node),
  atTopLevel: Boolean,
  currentTree: String,
  currentPaths: List[String],
  localScopes: List[State.Scope] = List[State.Scope]())

// TODO String should be changed to LocalName
case object State {
  type Scope = Map[String, Type]
}
