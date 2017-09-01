package flop.analysis.expressions

import org.scalatest._

import flop.analysis._
import flop.reading.Form

class BaseAnalysisSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(table: SymbolTable, state: State)

  def defaultAnalyzeFn(table: SymbolTable, state: State)(form: Form): Node =
    Node.NumLit(6.0f)

  def makeAnalyzeFn(nodes: List[Node]): (SymbolTable, State) => (Form => Node) = {
    var i = 0
    (st: SymbolTable, s: State) => {
      (f: Form) => {
        i += 1
        nodes(i - 1)
      }
    }
  }

  def withFixture(test: OneArgTest) = {
    val root = "user"
    val path = List[String]()
    val traits = Map[String, ModuleTree.Module.Trait]()
    val vars = Map[String, ModuleTree.Module.Var](
      "testnum" -> ModuleTree.Module.Var("testnum", Node.NumLit(6)),
    )
    val moduleName = "testm"
    val testModule = ModuleTree.Module(moduleName, root, path, traits, vars)
    val moduleTree = ModuleTree(root, Map(moduleName -> testModule))
    val symbolTable = SymbolTable.withRoot(moduleTree.name)
    val state = State(defaultAnalyzeFn, true, root, List(moduleName))

    val fixture = FixtureParam(symbolTable, state)

    withFixture(test.toNoArgTest(fixture))
  }

}
