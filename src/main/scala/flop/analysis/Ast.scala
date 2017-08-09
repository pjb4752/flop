package flop.analysis

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

  abstract class FnN(eType: Type.Fn) extends Node {
    val pTypes = eType.pTypes
    val rType = eType.rType
  }

  case class FlopFn(eType: Type.Fn, params: Params, expr: Node) extends FnN(eType)

  abstract class LuaFn(eType: Type.Fn) extends FnN(eType) {
    val name: String
    val arity = pTypes.length
  }

  case class LuaIFn(eType: Type.Fn, val name: String) extends LuaFn(eType)
  case class LuaPFn(eType: Type.Fn, val name: String) extends LuaFn(eType)

  abstract class ApplyN(args: List[Node], eType: Type) extends Node

  case class LuaApply(fn: LuaFn, args: List[Node], eType: Type)
    extends ApplyN(args, eType)

  case class FlopApply(name: String, args: List[Node], eType: Type)
    extends ApplyN(args, eType)

  case class FnDef(traitName: SymLit, fnName: SymLit, eType: Type) extends Node

  case class TraitN(name: SymLit, fnDefs: List[FnDef]) extends Node {
    val eType = Type.Trait
  }

  case class TraitImpl(targetType: Type, traitName: SymLit,
      fnImpls: Map[SymLit, FnN]) extends Node {
    val eType = Type.Trait
  }
}
