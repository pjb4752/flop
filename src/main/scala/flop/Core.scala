package flop

object Core {

  val specialForms = List("def", "fn", "if", "let")

  val builtins = Map[String, Node](
    "true" -> Node.TrueLit,
    "false" -> Node.FalseLit,
    "+" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Number, "+"),
    "-" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Number, "-"),
    "*" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Number, "*"),
    "/" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Number, "/"),
    ">" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Boolean, ">"),
    "<" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Boolean, "<"),
    "=" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Boolean, "=="),
    ">=" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Boolean, ">="),
    "<=" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Boolean, "<="),
    "<>" -> Node.LuaIFn(List(Type.Number, Type.Number), Type.Boolean, "~="),
    "print" -> Node.LuaPFn(List(Type.String), Type.Unit, "print")
  )

  val reserved = (builtins.keys ++ specialForms).toSet
}
