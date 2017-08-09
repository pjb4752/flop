package flop.analysis

sealed trait Type

object Type {
  case object Trait extends Type
  case object Self extends Type

  case object Unit extends Type

  case object Boolean extends Type

  case object Number extends Type
  case object String extends Type
  case object Symbol extends Type

  abstract class Fn(val pTypes: List[Type], val rType: Type) extends Type

  case class LuaFn(override val pTypes: List[Type], override val rType: Type)
      extends Fn(pTypes, rType)

  case class FreeFn(override val pTypes: List[Type], override val rType: Type)
      extends Fn(pTypes, rType)

  case class TraitFn(override val pTypes: List[Type], override val rType: Type)
      extends Fn(pTypes, rType)

  case object List extends Type
  case object Map extends Type
}
