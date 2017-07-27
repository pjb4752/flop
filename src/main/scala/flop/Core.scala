package flop

object Core {

  val exported = Map[String, Type.Fn](
    "+" -> Type.IFn("+"),
    "-" -> Type.IFn("-"),
    "*" -> Type.IFn("*"),
    "/" -> Type.IFn("/"),
    ">" -> Type.IFn(">"),
    "<" -> Type.IFn("<"),
    "=" -> Type.IFn("=="),
    ">=" -> Type.IFn(">="),
    "<=" -> Type.IFn("<="),
    "<>" -> Type.IFn("~="),
    "print" -> Type.PFn("print", 1)
  )
}
