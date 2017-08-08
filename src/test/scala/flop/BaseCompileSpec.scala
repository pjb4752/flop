package flop

import org.scalatest._

class BaseCompileSpec extends fixture.FunSpec with Matchers with Compilable {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val moduleTraits = Map[String, List[Node.FnDef]]()
    val moduleVars = Map[String, Type](
      "testnum" -> Type.Number,
      "testnum1" -> Type.Number,
      "testnum2" -> Type.Number,
      "testnum3" -> Type.Number,
      "value?" -> Type.FreeFn(List(Type.Number, Type.Number), Type.Boolean))
    val localVars = List[Map[String, Type]]()

    val analyzeState = Analyze.State(true, moduleTraits, moduleVars, localVars)
    val emitState = Emit.State(0, 0, 0)
    val compileFn = compile(analyzeState, emitState) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }
}
