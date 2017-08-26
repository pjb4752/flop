package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.reading.Form

class IfSpec extends BaseAnalysisSpec {

  describe("analyzing an if form") {
    describe("the form has the wrong number of arguments") {
      it("should throw a compile error") { f =>
        val test = Form.ListF(
          List(Form.SymF(">"), Form.SymF("a"), Form.SymF("b"))
        )
        val ifExpr = Form.SymF("a")
        val elseExpr = Form.SymF("b")
        val arguments = List(test, ifExpr, elseExpr)

        an [CompileError] should be thrownBy(
          If.analyze(f.table, f.state, arguments))
      }
    }

    describe("the test type is not boolean") {
      it("should raise an error") { f =>
        val test = Form.NumF(5.2f)
        val ifExpr = Form.SymF("a")
        val elseExpr = Form.SymF("b")
        val arguments = List(test, ifExpr, elseExpr)

        val newAnalyzeFn = makeAnalyzeFn(
          List(
            Node.NumLit(5.2f),
            Node.SymLit(Name.LocalName("a"), Type.Number),
            Node.SymLit(Name.LocalName("b"), Type.Number)
          )
        )

        val newState = f.state.copy(analyzeFn = newAnalyzeFn)

        an [CompileError] should be thrownBy(
          If.analyze(f.table, newState, arguments))
      }
    }

    describe("the branch types do not match") {
      it("should raise an error") { f =>
        val test = Form.SymF("test-thing?")
        val ifExpr = Form.SymF("a")
        val elseExpr = Form.SymF("b")
        val arguments = List(test, ifExpr, elseExpr)

        val newAnalyzeFn = makeAnalyzeFn(
          List(
            Node.FlopApply(
              Name.LocalName("test-thing?"),
              List[Node](),
              Type.Boolean
            ),
            Node.SymLit(Name.LocalName("a"), Type.Number),
            Node.SymLit(Name.LocalName("b"), Type.String)
          )
        )

        val newState = f.state.copy(analyzeFn = newAnalyzeFn)

        an [CompileError] should be thrownBy(
          If.analyze(f.table, newState, arguments))
      }
    }

    describe("the form is valid") {
      it("should return the correct ast") { f =>
        val test = Form.SymF("test-thing?")
        val ifExpr = Form.SymF("a")
        val elseExpr = Form.SymF("b")
        val arguments = List(test, ifExpr, elseExpr)

        val newAnalyzeFn = makeAnalyzeFn(
          List(
            Node.FlopApply(
              Name.LocalName("test-thing?"),
              List[Node](),
              Type.Boolean
            ),
            Node.SymLit(Name.LocalName("a"), Type.Number),
            Node.SymLit(Name.LocalName("b"), Type.Number)
          )
        )

        val newState = f.state.copy(analyzeFn = newAnalyzeFn)

        val ast = Node.IfN(
          Node.FlopApply(
            Name.LocalName("test-thing?"),
            List[Node](),
            Type.Boolean
          ),
          Node.SymLit(Name.LocalName("a"), Type.Number),
          Node.SymLit(Name.LocalName("b"), Type.Number),
          Type.Number
        )

        If.analyze(f.table, newState, arguments) should equal(ast)
      }
    }
  }
}
