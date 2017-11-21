package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.analysis.ModuleTree.{Module => FlopModule}
import flop.reading.Form
import flop.stdlib.Core

class ModuleSpec extends BaseAnalysisSpec {

  describe("analyzing a module form") {
    describe("the wrong number of arguments are given") {
      it("should throw a compile error") { f =>
        val args = Form.ListF(List[Form]())

        an [CompileError] should be thrownBy(
          Module.analyze(f.table, args))
      }
    }

    describe("the name is not qualified") {
      it("should throw a compile error") { f =>
        val name = Form.SymF("bar")
        val args = Form.ListF(List(Form.SymF("module"), name))

        an [CompileError] should be thrownBy(
          Module.analyze(f.table, args))
      }
    }

    describe("the name given contains blank paths") {
      it("should throw a compile error") { f =>
        val name = Form.SymF(".foo.bar")
        val args = Form.ListF(List(Form.SymF("module"), name))

        an [CompileError] should be thrownBy(
          Module.analyze(f.table, args))
      }
    }

    describe("the name is valid") {
      it("should return the correct module") { f =>
        val name = Form.SymF("foo.bar.baz")
        val args = Form.ListF(List(Form.SymF("module"), name))

        val actualName = Name.ModuleName("foo", List("bar"), "baz")
        Module.analyze(f.table, args) should equal(
          FlopModule.initial(actualName, Core.stdLibImports))
      }
    }

    describe("importing modules") {
      describe("the imported module is not abbreviated") {
        describe("the name is not fully qualified") {
          it("should throw an error") { f =>
            val name = Form.SymF("foo.bar.baz")
            val importName = Form.SymF("foobar")
            val args = Form.ListF(
              List(
                Form.SymF("module"), name,
                Form.ListF(List(Form.SymF("import"), importName))
              )
            )

            an [CompileError] should be thrownBy(
              Module.analyze(f.table, args))
          }
        }

        describe("the same implicit abbreviation is used twice") {
          it("should throw an error") { f =>
            val name = Form.SymF("foo.bar.baz")
            val importName = Form.SymF("foo.bar.quux")
            val args = Form.ListF(
              List(
                Form.SymF("module"), name,
                Form.ListF(List(Form.SymF("import"), importName, importName))
              )
            )

            an [CompileError] should be thrownBy(
              Module.analyze(f.table, args))
          }
        }

        describe("the name is valid") {
          it("add the module import") { f =>
            val name = Form.SymF("foo.bar.baz")
            val importName = Form.SymF("foo.bar.quux")
            val args = Form.ListF(
              List(
                Form.SymF("module"), name,
                Form.ListF(List(Form.SymF("import"), importName))
              )
            )

            val mName = Name.ModuleName("foo", List("bar"), "quux")
            val actualName = Name.ModuleName("foo", List("bar"), "baz")
            val allImports = Core.stdLibImports + (mName.name -> mName)
            Module.analyze(f.table, args) should equal(
              FlopModule.initial(actualName, allImports))
          }
        }
      }

      describe("the imported module is abbreviated") {
        describe("the name is not fully qualified") {
          it("should throw an error") { f =>
            val name = Form.SymF("foo.bar.baz")
            val importName = Form.SymF("foobar")
            val importAbbr = Form.SymF("abbr")
            val importArgs = Form.ListF(List(importName, importAbbr))
            val args = Form.ListF(
              List(
                Form.SymF("module"), name,
                Form.ListF(List(Form.SymF("import"), importArgs))
              )
            )

            an [CompileError] should be thrownBy(
              Module.analyze(f.table, args))
          }
        }

        describe("the same explicit abbreviation is used twice") {
          it("should throw an error") { f =>
            val name = Form.SymF("foo.bar.baz")
            val importName1 = Form.SymF("foo.bar.quux")
            val importName2 = Form.SymF("other.bar.quux")
            val importAbbr = Form.SymF("abbr")
            val importArgs1 = Form.ListF(List(importName1, importAbbr))
            val importArgs2 = Form.ListF(List(importName2, importAbbr))
            val args = Form.ListF(
              List(
                Form.SymF("module"), name,
                Form.ListF(List(Form.SymF("import"), importArgs1, importArgs2))
              )
            )

            an [CompileError] should be thrownBy(
              Module.analyze(f.table, args))
          }
        }

        describe("the name is valid") {
          it("add the module import") { f =>
            val name = Form.SymF("foo.bar.baz")
            val importName = Form.SymF("foo.bar.quux")
            val importAbbr = Form.SymF("abbr")
            val importArgs = Form.ListF(List(importName, importAbbr))
            val args = Form.ListF(
              List(
                Form.SymF("module"), name,
                Form.ListF(List(Form.SymF("import"), importArgs))
              )
            )

            val mName = Name.ModuleName("foo", List("bar"), "quux")
            val actualName = Name.ModuleName("foo", List("bar"), "baz")
            val allImports = Core.stdLibImports + ("abbr" -> mName)
            Module.analyze(f.table, args) should equal(
              FlopModule.initial(actualName, allImports))
          }
        }
      }
    }
  }
}
