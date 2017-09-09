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

  object ModuleName {

    def nest(modName: ModuleName, value: String): ModuleName = {
      modName.copy(paths = modName.paths :+ modName.name, name = value)
    }

    def flatten(modName: ModuleName): (ModuleName, String) = {
      print(modName)
      val name :: paths = modName.paths.reverse
      (modName.copy(paths = paths, name = name), modName.name)
    }
  }
}
