package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.analysis.ModuleTree.{Module => FlopModule}
import flop.reading.Form

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
          FlopModule.initial(actualName))
      }
    }
  }
}
