package flop

sealed trait Form

object Form {

  case class NumF(value: Int) extends Form
  case class StrF(value: String) extends Form
  case class SymF(value: String) extends Form
  case class ListF(value: List[Form]) extends Form
}
