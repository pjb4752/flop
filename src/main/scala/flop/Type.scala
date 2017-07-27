package flop

sealed trait Type

object Type {

  abstract class Fn(val name: String) extends Type {
    val arity: Int
  }

  case class IFn(override val name: String) extends Fn(name) {
    val arity = 2
  }

  case class PFn(override val name: String, val arity: Int) extends Fn(name)
}
