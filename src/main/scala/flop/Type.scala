package flop

sealed trait Type

object Type {

  case class Fn(val arity: Int) extends Type
}
