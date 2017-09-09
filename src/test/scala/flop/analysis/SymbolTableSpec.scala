package flop.analysis

import org.scalatest._

import flop.analysis._
import flop.reading._
import flop.stdlib.Core

class SymbolTableSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(table: SymbolTable, state: State)

  def defaultAnalyzeFn(table: SymbolTable, state: State)(form: Form): Node =
    Node.TrueLit

  def withFixture(test: OneArgTest) = {
    val root = "root"
    val table = SymbolTable.withRoot(root)
    val path = List[String]("core")
    val imports = Map[String, Name.ModuleName]()
    val traits = Map[String, ModuleTree.Module.Trait]()
    val vars = Map[String, ModuleTree.Module.Var](
      "testnum" -> ModuleTree.Module.Var("testnum", Node.NumLit(6)),
    )
    val moduleName = Name.ModuleName(root, path, "testm")
    val testModule = ModuleTree.Module(moduleName, imports, traits, vars)
    val moduleTree = ModuleTree(root,
      Map(
        "core" -> ModuleTree.SubTree(
          "core",
          Map(moduleName.name -> testModule)
        )
      )
    )
    val finalTable = table.copy(trees = table.trees + (root -> moduleTree))
    val state = State(defaultAnalyzeFn, true, testModule)
    val fixture = FixtureParam(finalTable, state)

    withFixture(test.toNoArgTest(fixture))
  }

  describe("finding a module in the table") {
    describe("the module exists") {
      it("should return the module") { f =>
        SymbolTable.findModule(
          f.table, Core.rootName, Core.commonPath
        ) should equal(Some(Core.commonModule))
      }
    }

    describe("the module does not exist") {
      it("should not return the module") { f =>
        SymbolTable.findModule(
          f.table, Core.rootName, List("foo", "bar")
        ) should equal(None)
      }
    }

    describe("looking up names") {
      describe("the name is a literal") {
        it("should return the name") { f =>
          SymbolTable.lookupName(
            f.table, f.state, "true"
          ) should equal(Some(Name.LiteralName("true")))
        }
      }

      describe("the name is qualified") {
        describe("the name exists") {
          it("should return the name") { f =>
            SymbolTable.lookupName(
              f.table, f.state, "flop.core.common.+",
            ) should equal(
              Some(Name.ModuleName("flop", List("core", "common"), "+"))
            )
          }
        }

        describe("the name is a reserved word") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "flop.core.common.def")
            )
          }
        }

        describe("the module is not valid") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "flop.core.other.+")
            )
          }
        }

        describe("the name does not exist") {
          it("should return None") { f =>
            SymbolTable.lookupName(
              f.table, f.state, "flop.core.common.thing",
            ) should equal(None)
          }
        }
      }

      describe("the name is unqualified") {
        describe("the name exists") {
          describe("the name is local") {
            it("should return the name") { f =>
              val newState = f.state.copy(
                localScopes = List(Map("+" -> Type.Number)))

              SymbolTable.lookupName(
                f.table, newState, "+",
              ) should equal(
                Some(Name.LocalName("+"))
              )
            }
          }

          describe("the name is in the current module") {
            it("should return the name") { f =>
              SymbolTable.lookupName(
                f.table, f.state, "testnum",
              ) should equal(
                Some(Name.ModuleName("root", List("core", "testm"), "testnum"))
              )
            }
          }

          describe("the name is global") {
            it("should return the name") { f =>
              SymbolTable.lookupName(
                f.table, f.state, "+",
              ) should equal(
                Some(Name.ModuleName("flop", List("core", "common"), "+"))
              )
            }
          }
        }

        describe("the name is a reserved word") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "def")
            )
          }
        }

        describe("the name is undefined") {
          it("should return None") { f =>
            SymbolTable.lookupName(f.table, f.state, "heya") should equal(None)
          }
        }
      }
    }

    describe("looking up vars") {
      describe("the var exists") {
        it("should return the var") {

        }
      }

      describe("the var does not exist") {
        it("should return None") {

        }
      }

      describe("the name is a valid var name") {
        it("should throw a compile error") {

        }
      }
    }
  }
}
