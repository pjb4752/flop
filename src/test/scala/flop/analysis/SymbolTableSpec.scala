package flop.analysis

import org.scalatest._

import flop.analysis._
import flop.reading._
import flop.stdlib.Core
import flop.stdlib.core.Common

class SymbolTableSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(
    table: SymbolTable,
    state: State,
    testModule: ModuleTree.Module
  )

  def defaultAnalyzeFn(table: SymbolTable, state: State)(form: Form): Node =
    Node.TrueLit

  def withFixture(test: OneArgTest) = {
    val root = "root"
    val table = SymbolTable.withRoot(root)
    val path = List[String]("core")
    val imports = Core.stdLibImports
    val traits = Map[String, ModuleTree.Module.Trait]()
    val traitImpls = Map[ModuleTree.Module.TraitFn, Node.FnN]()
    val vars = Map[String, ModuleTree.Module.Var](
      "testnum" -> ModuleTree.Module.Var("testnum", Node.NumLit(6)),
    )
    val moduleName = Name.ModuleName(root, path, "testm")
    val testModule = ModuleTree.Module(moduleName, imports, traits, traitImpls, vars)
    val moduleTree = ModuleTree(root,
      Map(
        "core" -> ModuleTree.SubTree(
          "core",
          Map(moduleName.name -> testModule)
        )
      )
    )
    val finalTable = table.copy(trees = table.trees + (root -> moduleTree))
    val state = State(defaultAnalyzeFn, true, moduleName)
    val fixture = FixtureParam(finalTable, state, testModule)

    withFixture(test.toNoArgTest(fixture))
  }

  describe("finding a module in the table") {
    describe("the module exists") {
      it("should return the module") { f =>
        SymbolTable.findModule(
          f.table, Core.rootName, Common.path
        ) should equal(Some(Common.module))
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
              f.table, f.state, "flopcore.core.common/+",
            ) should equal(
              Some(
                Name.TraitFnName(
                  Name.ModuleName("flopcore", List("core", "common"), "Numeric"),
                  "+"
                )
              )
            )
          }
        }

        describe("the name is a reserved word") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "flopcore.core.common/def")
            )
          }
        }

        describe("the module is not valid") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "flopcore.core.other.+")
            )
          }
        }

        describe("the name does not exist") {
          it("should return None") { f =>
            SymbolTable.lookupName(
              f.table, f.state, "flopcore.core.common/thing",
            ) should equal(None)
          }
        }
      }

      describe("the name is aliased") {
        describe("the name exists") {
          it("should return the name") { f =>
            SymbolTable.lookupName(
              f.table, f.state, "list/head",
            ) should equal(
              Some(
                Name.ModuleName("flopcore", List("core", "list"), "head"),
              )
            )
          }
        }

        describe("the name is a reserved word") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "list/def")
            )
          }
        }

        describe("the alias is not valid") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupName(f.table, f.state, "hi/head")
            )
          }
        }

        describe("the name is not valid") {
          it("should return None") { f =>
            SymbolTable.lookupName(
              f.table, f.state, "list/foo"
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
            describe("the name is a var") {
              val path = List("core", "common")
              val moduleName = Name.ModuleName("flopcore", path, "print")

              it("should return the name") { f =>
                SymbolTable.lookupName(
                  f.table, f.state, "print",
                ) should equal(
                  Some(moduleName)
                )
              }
            }

            describe("the name is a trait fn") {
              it("should return the name") { f =>
                val path = List("core", "common")
                val moduleName = Name.ModuleName("flopcore", path, "Numeric")

                SymbolTable.lookupName(
                  f.table, f.state, "+",
                ) should equal(
                  Some(Name.TraitFnName(moduleName, "+"))
                )
              }
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
        it("should return the var") { f =>
          SymbolTable.lookupVar(
            f.table, Name.ModuleName("root", List("core", "testm"), "testnum")
          ) should equal(
            Some(ModuleTree.Module.Var("testnum", Node.NumLit(6.0f)))
          )
        }
      }

      describe("the var does not exist") {
        it("should return None") { f =>
          SymbolTable.lookupVar(
            f.table, Name.ModuleName("flopcore", List("core", "common"), "thingy")
          ) should equal(None)
        }
      }

      describe("the name is a valid var name") {
        it("should throw a compile error") { f =>
          an [CompileError] should be thrownBy(
            SymbolTable.lookupVar(f.table, Name.LocalName("thingy"))
          )
        }
      }
    }

    describe("looking up types") {
      describe("the name is a literal name") {
        describe("the name exists") {
          it("should return the type") { f =>
            SymbolTable.lookupType(
              f.table, f.state, Name.LiteralName("true")
            ) should equal(Node.TrueLit.eType)
          }
        }

        describe("the name does not exist") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupType(
                f.table, f.state, Name.LiteralName("howdy"))
            )
          }
        }
      }

      describe("the name is a local name") {
        describe("the type exists") {
          it("should return the type") { f =>
            val newState = f.state.copy(
              localScopes = List(Map("+" -> Type.Number)))

            SymbolTable.lookupType(
              f.table, newState, Name.LocalName("+")
            ) should equal(Type.Number)
          }
        }

        describe("the name does not exist") {
          it("should throw a compile error") { f =>
            an [CompileError] should be thrownBy(
              SymbolTable.lookupType(
                f.table, f.state, Name.LocalName("howdy"))
            )
          }
        }
      }

      describe("the name is a module name") {
        describe("the type exists") {
          it("should return the type") { f =>
            val path = List("core", "testm")

            SymbolTable.lookupType(
              f.table, f.state, Name.ModuleName("root", path, "testnum")
            ) should equal(Type.Number)
          }
        }

        describe("the name does not exist") {
          it("should throw a compile error") { f =>
            val path = List("core", "testm")

            an [CompileError] should be thrownBy(
              SymbolTable.lookupType(
                f.table, f.state, Name.ModuleName("root", path, "howdy"))
            )
          }
        }
      }
    }

    describe("adding a module") {
      describe("the tree does not exist") {
        it("should add the module") { f =>
          val newName = Name.ModuleName("foo", List("bar"), "baz")
          val newModule = ModuleTree.Module.initial(newName, Core.stdLibImports)
          val newTree = ModuleTree(
            "foo",
            Map(
              "bar" -> ModuleTree.SubTree(
                "bar",
                Map("baz" -> newModule)
              )
            )
          )

          SymbolTable.addModule(f.table, newModule) should equal(
            f.table.copy(trees = f.table.trees + ("foo" -> newTree)))
        }
      }

      describe("the tree exists") {
        it("should add the module") { f =>
          val newName = Name.ModuleName("root", List("core"), "foo")
          val newModule = ModuleTree.Module.initial(newName, Core.stdLibImports)
          val newTree = ModuleTree(
            "root",
            Map(
              "core" -> ModuleTree.SubTree(
                "core",
                Map(
                  "testm" -> f.testModule,
                  "foo" -> newModule
                )
              ),
            )
          )

          SymbolTable.addModule(f.table, newModule) should equal(
            f.table.copy(trees = f.table.trees + ("root" -> newTree)))
        }
      }
    }
  }
}
