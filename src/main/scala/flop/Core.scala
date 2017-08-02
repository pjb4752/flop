package flop

object Core {

  val symbols = Map[String, Node.LuaFn](
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
}
