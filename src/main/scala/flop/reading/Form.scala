package flop.reading

sealed trait Form

object Form {

  case class NumF(value: Float) extends Form
  case class StrF(value: String) extends Form
  case class SymF(value: String) extends Form
  case class ListF(value: List[Form]) extends Form
  case class MapF(value: Map[Form, Form]) extends Form
}
