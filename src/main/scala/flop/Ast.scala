package flop

sealed trait Node {
  val state: State
}

object Node {

  case class NumLit(val state: State, value: Float) extends Node
  case class StrLit(val state: State, value: String) extends Node
  case class SymLit(val state: State, val value: String) extends Node
  case class ListLit(val state: State, value: List[Node]) extends Node

  case class DefN(val state: State, name: SymLit, expr: Node) extends Node
  case class LetN(val state: State, bindings: List[(SymLit, Node)], exprs: Node) extends Node
  case class IfN(val state: State, test: Node, ifExpr: Node, elseExpr: Node) extends Node
  case class ApplyN(val state: State, fn: Type.Fn, args: List[Node]) extends Node
}
