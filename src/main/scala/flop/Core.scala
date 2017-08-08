package flop

object Core {

  val specialForms = List("def", "fn", "if", "let")

  val builtins = Map[String, Node](
    "true" -> Node.TrueLit,
    "false" -> Node.FalseLit,
    "+" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "+"),
    "-" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "-"),
    "*" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "*"),
    "/" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Number), "/"),
    ">" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), ">"),
    "<" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "<"),
    "=" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "=="),
    ">=" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), ">="),
    "<=" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "<="),
    "<>" -> Node.LuaIFn(Type.LuaFn(List(Type.Number, Type.Number), Type.Boolean), "~="),
    "print" -> Node.LuaPFn(Type.LuaFn(List(Type.String), Type.Unit), "print"),
    "num_to_s" -> Node.LuaPFn(Type.TraitFn(List(Type.Number), Type.String), "num_to_s")
  )

  val showStrType = Type.TraitFn(List(Type.Self), Type.String)

  val builtinTraits = Map[String, List[Node.FnDef]](
    "show" -> List(
      Node.FnDef(
        Node.SymLit("show", Type.Trait),
        Node.SymLit("str", showStrType),
        showStrType
      )
    )
  )

  val numShowImpl = Node.TraitImpl(
      Type.Number,
      Node.SymLit("show", Type.Trait),
      Map[Node.SymLit, Node.FnN](
        Node.SymLit("str", showStrType) ->
        Node.LuaPFn(showStrType, "num_to_s")
      )
  )

  val boolShowImpl = Node.TraitImpl(
    Type.Boolean,
    Node.SymLit("show", Type.Trait),
    Map[Node.SymLit, Node.FnN](
      Node.SymLit("str", showStrType) ->
      Node.LuaPFn(showStrType, "bool_to_s")
    )
  )

  val traitImpls = Map[String, Map[Type, Node.TraitImpl]](
    "str" -> Map(
      Type.Number -> numShowImpl,
      Type.Boolean -> boolShowImpl
    )
  )

  val reserved = (
    builtins.keys ++
    specialForms ++
    builtinTraits.flatMap(_._2).map({ case Node.FnDef(_, n, _) => n.value })
  ).toSet
}
