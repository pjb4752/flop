package flop

sealed trait Type

object Type {

  case object Unit extends Type

  case object Boolean extends Type

  case object Number extends Type

  case object String extends Type

  case object Symbol extends Type

  case object List extends Type

  case object Map extends Type

  case class Fn(pTypes: List[Type], rType: Type) extends Type
}
