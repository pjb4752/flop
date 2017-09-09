package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.reading.Form

class DefSpec extends BaseAnalysisSpec {

  describe("analyzing a def form") {
    describe("the form is not at top level") {
      it("should raise a compile error") { f =>
        val name = Form.SymF("variable")
        val expr = Form.NumF(5.2f)
        val args = List(name, expr)
        val newState = f.state.copy(atTopLevel = false)

        an [CompileError] should be thrownBy(
          Def.analyze(f.table, newState, args))
      }
    }

    describe("there is a syntax error") {
      it("should raise a compile error") { f =>
        val name = Form.SymF("variable")
        val args = List(name)

        an [CompileError] should be thrownBy(
          Def.analyze(f.table, f.state, args))
      }
    }

    describe("the name is not a symbol") {
      it("should raise a compile error") { f =>
        val name = Form.StrF("variable")
        val expr = Form.NumF(5.2f)
        val args = List(name, expr)

        an [CompileError] should be thrownBy(
          Def.analyze(f.table, f.state, args))
      }
    }

    describe("the name is fully qualified") {
      it("should raise a compile error") { f =>
        val name = Form.SymF("core.var")
        val expr = Form.NumF(5.2f)
        val args = List(name, expr)

        an [CompileError] should be thrownBy(
          Def.analyze(f.table, f.state, args))
      }
    }

    describe("the name is a reserved name") {
      it("should raise a compile error") { f =>
        val name = Form.SymF("let")
        val expr = Form.NumF(5.2f)
        val args = List(name, expr)

        an [CompileError] should be thrownBy(
          Def.analyze(f.table, f.state, args))
      }
    }

    describe("the form is valid") {
      it("should return the proper ast for the def") { f =>
        val name = Form.SymF("foo")
        val expr = Form.NumF(5.2f)
        val args = List(name, expr)

        val ast = Node.DefN(
          Node.SymLit(
            Name.ModuleName("user", List("core", "testm"), "foo"),
            Type.Number
          ),
          Node.NumLit(6.0f),
          Type.Number
        )

        Def.analyze(f.table, f.state, args) should equal(ast)
      }
    }
  }
}
