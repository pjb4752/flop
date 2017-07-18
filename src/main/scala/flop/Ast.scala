package flop

sealed trait Node

object Node {
  case object UnitN extends Node

  case class NumLit(value: Float) extends Node
  case class StrLit(value: String) extends Node
  case class SymLit(value: String) extends Node
  case class ListLit(value: List[Node]) extends Node

  case class DefN(name: SymLit, expr: Node) extends Node
  case class LetN(bindings: List[(SymLit, Node)], exprs: Node) extends Node
  case class IfN(test: Node, ifExpr: Node, elseExpr: Node) extends Node
  case class ApplyN(fn: Type.Fn, args: List[Node]) extends Node
}
