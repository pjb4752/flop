package flop

object Core {

  val exported = Map[String, Type.Fn](
    "+" -> Type.Fn(2),
    "-" -> Type.Fn(2),
    "*" -> Type.Fn(2),
    "/" -> Type.Fn(2))
}
