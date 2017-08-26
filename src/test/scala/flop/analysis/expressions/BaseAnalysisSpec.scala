package flop.analysis.expressions

import org.scalatest._

import flop.analysis._
import flop.reading.Form

class BaseAnalysisSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(table: SymbolTable, state: State)

  def defaultAnalyzeFn(table: SymbolTable, state: State)(form: Form): Node =
    Node.NumLit(6.0f)

  def withFixture(test: OneArgTest) = {
    val traits = Map[String, ModuleTree.Module.Trait]()
    val vars = Map[String, ModuleTree.Module.Var](
      "testnum" -> ModuleTree.Module.Var("testnum", Node.NumLit(6)),
    )
    val testModule = ModuleTree.Module("testm", traits, vars)
    val moduleTree = ModuleTree("user", Map("testm" -> testModule))
    val symbolTable = SymbolTable.withRoot(moduleTree.name)
    val state = State(defaultAnalyzeFn, true, "user", List("testm"))

    val fixture = FixtureParam(symbolTable, state)

    withFixture(test.toNoArgTest(fixture))
  }

}
