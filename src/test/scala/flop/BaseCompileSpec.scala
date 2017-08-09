package flop

import org.scalatest._

import analysis.{Analysis, Node, State => AState, Type}
import backend.{Backend, State => EState}
import reading.{Reading}

class BaseCompileSpec extends fixture.FunSpec with Matchers {

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

    val analysisState = AState(true, moduleTraits, moduleVars, localVars)
    val emitState = EState.initial
    val compileFn = compile(analysisState, emitState) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }

  private def compile(analysisState: AState, emitState: EState)(source: String): String = {
    val forms = Reading.read(source)
    val (_, ast) = Analysis.analyze(analysisState, forms)

    Backend.emit(ast, emitState)
  }
}
