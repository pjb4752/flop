package flop.integration

import org.scalatest._

import flop.analysis._
import flop.analysis.ModuleTree._
import flop.backend.{Backend, State => EState}
import flop.reading.{Reading}

class BaseCompileSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val root = "user"
    val path = List[String]("core")
    val imports = Map[String, Name.ModuleName]()
    val traits = Map[String, Module.Trait]()
    val vars = Map[String, Module.Var](
      "testnum" -> Module.Var("testnum", Node.NumLit(6)),
      "testnum1" -> Module.Var("testnum1", Node.NumLit(7)),
      "testnum2" -> Module.Var("testnum2", Node.NumLit(8)),
      "testnum3" -> Module.Var("testnum3", Node.NumLit(9)),
      "value?" -> Module.Var("value?",
          Node.LuaPFn(
            Type.FreeFn(List(Type.Number, Type.Number), Type.Boolean),
            "value__question"
          )
        )
    )
    val moduleName = Name.ModuleName(root, path, "testm")
    val testModule = Module(moduleName, imports, traits, vars)
    val moduleTree = ModuleTree(root,
      Map(
        "core" -> SubTree(
          "core",
          Map(moduleName.name -> testModule)
        )
      )
    )
    val symbolTable = SymbolTable.withRoot(moduleTree.name)
    val finalTable = symbolTable.copy(
      trees = symbolTable.trees + (root -> moduleTree))

    val compileFn = compile(finalTable, testModule) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }

  private def compile(table: SymbolTable, module: Module)(source: String): String = {
    val forms = Reading.read(source)
    val (_, ast) = Analysis.analyze(table, module, forms)

    Backend.emit(ast).mkString("\n")
  }
}
