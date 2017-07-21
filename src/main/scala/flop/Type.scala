package flop

sealed trait Type

object Type {

  abstract class Fn(val name: String) extends Type {
    val arity: Int
  }

  case class InfixFn(override val name: String) extends Fn(name) {
    val arity = 2
  }

  case class PrefixFn(override val name: String, val arity: Int) extends Fn(name)
}
