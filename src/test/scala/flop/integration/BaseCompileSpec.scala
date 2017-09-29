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
    val importedModule = makeImportedModule(root)
    val currentModule = makeCurrentModule(root, importedModule)
    val moduleTree = ModuleTree(root,
      Map(
        "core" -> SubTree(
          "core",
          Map(
            importedModule.name.name -> importedModule,
            currentModule.name.name -> currentModule
          )
        )
      )
    )
    val symbolTable = SymbolTable.withRoot(moduleTree.name)
    val finalTable = symbolTable.copy(
      trees = symbolTable.trees + (root -> moduleTree))

    val compileFn = compile(finalTable, currentModule) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }

  private def makeImportedModule(root: String): Module = {
    val path = List[String]("core")
    val imports = Map[String, Name.ModuleName]()
    val traits = Map[String, Module.Trait]()
    val traitImpls = Map[ModuleTree.Module.TraitFn, Node.FnN]()
    val vars = Map[String, Module.Var](
      "six" -> Module.Var("six", Node.NumLit(6)),
    )
    val moduleName = Name.ModuleName(root, path, "foomod")
    Module(moduleName, imports, traits, traitImpls, vars)
  }

  private def makeCurrentModule(root: String, imported: Module): Module = {
    val path = List[String]("core")
    val imports = Map(imported.name.name -> imported.name)
    val traits = Map[String, Module.Trait]()
    val traitImpls = Map[ModuleTree.Module.TraitFn, Node.FnN]()
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
    Module(moduleName, imports, traits, traitImpls, vars)
  }

  private def compile(table: SymbolTable, module: Module)(source: String): String = {
    val forms = Reading.read(source)
    val (_, ast) = Analysis.analyze(table, module, forms)

    val state = EState(module, 0, 0, 0)
    Backend.emit(state, ast).mkString("\n")
  }
}
