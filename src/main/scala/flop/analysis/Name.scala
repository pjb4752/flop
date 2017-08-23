package flop.analysis

// TODO merge this in with Node.SymLit?
sealed trait Name {
  val name: String
}

object Name {
  case class LiteralName(val name: String) extends Name

  case class ModuleName(val tree: String, val paths: List[String],
      val name: String) extends Name

  case class LocalName(val name: String) extends Name
}
