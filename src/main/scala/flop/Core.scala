package flop

object Core {

  val exported = Map[String, Type.Fn](
    "+" -> Type.InfixFn("+"),
    "-" -> Type.InfixFn("-"),
    "*" -> Type.InfixFn("*"),
    "/" -> Type.InfixFn("/"),
    ">" -> Type.InfixFn(">"),
    "<" -> Type.InfixFn("<"),
    "=" -> Type.InfixFn("=="),
    ">=" -> Type.InfixFn(">="),
    "<=" -> Type.InfixFn("<="),
    "<>" -> Type.InfixFn("~="),
    "print" -> Type.PrefixFn("print", 1)
  )
}
