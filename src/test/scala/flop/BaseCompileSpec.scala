package flop

import org.scalatest._

class BaseCompileSpec extends fixture.FunSpec with Matchers with Compilable {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val state = Emit.State()
    val compileFn = compile(state) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }
}
