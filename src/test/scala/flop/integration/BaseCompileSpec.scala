package flop.integration

import org.scalatest._

import flop.analysis.{Analysis, Module, Node, State => AState, Type}
import flop.backend.{Backend, State => EState}
import flop.reading.{Reading}

class BaseCompileSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val traits = Map[String, Module.Trait]()
    val vars = Map[String, Module.Var](
      "testnum" -> Module.Var("testnum", Type.Number),
      "testnum1" -> Module.Var("testnum1", Type.Number),
      "testnum2" -> Module.Var("testnum2", Type.Number),
      "testnum3" -> Module.Var("testnum3", Type.Number),
      "value?" -> Module.Var("value?",
          Type.FreeFn(List(Type.Number, Type.Number), Type.Boolean)
        )
    )
    val testModule = Module("testm", traits, vars)

    val modules = Map[String, Module]("testm" -> testModule)

    val analysisState = AState(true, testModule, modules)
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
