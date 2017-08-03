package flop

sealed trait Node {
  val eType: Type
}

object Node {
  type Binding = (SymLit, Node)
  type Bindings = List[Binding]

  type Param = (SymLit, Type)
  type Params = List[Param]

  sealed trait BoolLit extends Node {
    val eType = Type.Boolean
  }

  case object TrueLit extends BoolLit
  case object FalseLit extends BoolLit

  case class NumLit(value: Float) extends Node {
    val eType = Type.Number
  }

  case class StrLit(value: String) extends Node {
    val eType = Type.String
  }

  case class SymLit(val value: String, val eType: Type) extends Node

  case class ListLit(value: List[Node]) extends Node {
    val eType = Type.List
  }

  case class MapLit(value: Map[Node, Node]) extends Node {
    val eType = Type.Map
  }

  case class DefN(name: SymLit, expr: Node, eType: Type) extends Node

  case class LetN(bindings: Bindings, expr: Node, eType: Type) extends Node

  case class IfN(test: Node, ifExpr: Node, elseExpr: Node, eType: Type) extends Node

  abstract class FnN(pTypes: List[Type], rType: Type) extends Node {
    val eType = Type.Fn(pTypes, rType)
  }

  case class FlopFn(params: Params, rType: Type, expr: Node)
      extends FnN(params.map(_._2), rType)

  abstract class LuaFn(pTypes: List[Type], rType: Type, val name: String)
      extends FnN(pTypes, rType) {
    val arity = pTypes.length
  }

  case class LuaIFn(pTypes: List[Type], rType: Type, override val name: String)
      extends LuaFn(pTypes, rType, name)

  case class LuaPFn(pTypes: List[Type], rType: Type, override val name: String)
      extends LuaFn(pTypes, rType, name)

  abstract class ApplyN(args: List[Node], eType: Type) extends Node

  case class LuaApply(fn: LuaFn, args: List[Node], eType: Type)
    extends ApplyN(args, eType)

  case class FlopApply(name: String, args: List[Node], eType: Type)
    extends ApplyN(args, eType)
}
