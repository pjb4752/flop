package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.reading.Form

class LetSpec extends BaseAnalysisSpec {

  describe("analyzing a let form") {
    describe("the form has the wrong number of arguments") {
      it("should throw a compile error") { f =>
        val bindings = Form.ListF(
          List(Form.SymF("a"), Form.NumF(3.9f))
        )
        val args = List(bindings)

        an [CompileError] should be thrownBy(
          Let.analyze(f.table, f.state, args))
      }
    }

    describe("analyzing binding form") {
      describe("the form is not a list") {
        it("should throw a compile error") { f =>
          val bindings = Form.SymF("a")
          val expr = Form.NumF(6.0f)
          val args = List(bindings, expr)

          an [CompileError] should be thrownBy(
            Let.analyze(f.table, f.state, args))
        }
      }

      describe("the number of terms in the list is odd") {
        it("should throw a compile error") { f =>
          val bindings = Form.ListF(
            List(Form.SymF("a"), Form.NumF(3.9f), Form.SymF("b"))
          )
          val expr = Form.NumF(6.0f)
          val args = List(bindings, expr)

          an [CompileError] should be thrownBy(
            Let.analyze(f.table, f.state, args))
        }
      }

      describe("the binding name is not a symbol") {
        it("should throw a compile error") { f =>
          val bindings = Form.ListF(
            List(Form.StrF("a"), Form.NumF(3.9f))
          )
          val expr = Form.NumF(6.0f)
          val args = List(bindings, expr)

          an [CompileError] should be thrownBy(
            Let.analyze(f.table, f.state, args))
        }
      }

      describe("the binding name is a reserved value") {
        it("should throw a compile error") { f =>
          val bindings = Form.ListF(
            List(Form.SymF("let"), Form.NumF(3.9f))
          )
          val expr = Form.NumF(6.0f)
          val args = List(bindings, expr)

          an [CompileError] should be thrownBy(
            Let.analyze(f.table, f.state, args))
        }
      }
    }

    describe("the form is valid") {
      it("should return the correct ast") { f =>
        val bindings = Form.ListF(
         List(Form.SymF("foo"), Form.NumF(3.9f))
        )
        val expr = Form.NumF(6.0f)
        val args = List(bindings, expr)

        val ast = Node.LetN(
          List(
            (
              Node.SymLit(Name.LocalName("foo"), Type.Number),
              Node.NumLit(6.0f)
            )
          ),
          Node.NumLit(6.0f),
          Type.Number
        )

        Let.analyze(f.table, f.state, args) should equal(ast)
      }
    }
  }
}
