package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.reading.Form

class FnSpec extends BaseAnalysisSpec {

  describe("analyzing a function form") {
    describe("the form has the wrong number of arguments") {
      it("should throw a compile error") { f =>
        val returnType = Form.SymF("num")
        val params = Form.MapF(
          Map(Form.SymF("a") -> Form.SymF("num"))
        )
        val args = List(returnType, params)

        an [CompileError] should be thrownBy(
          Fn.analyze(f.table, f.state, args))
      }
    }

    describe("analyzing function parameters") {
      describe("the function params are not a map") {
        it("should throw a compile error") { f =>
          val returnType = Form.SymF("num")
          val params = Form.ListF(List[Form]())
          val expr = Form.NumF(5.2f)
          val args = List(returnType, params, expr)

          an [CompileError] should be thrownBy(
            Fn.analyze(f.table, f.state, args))
        }
      }

      describe("a function parameter is not a symbol") {
        it("should throw a compile error") { f =>
          val returnType = Form.SymF("num")
          val params = Form.MapF(
            Map(Form.StrF("a") -> Form.SymF("num"))
          )
          val expr = Form.NumF(5.2f)
          val args = List(returnType, params, expr)

          an [CompileError] should be thrownBy(
            Fn.analyze(f.table, f.state, args))
        }
      }

      describe("a function parameter is a reserved name") {
        it("should throw a compile error") { f =>
          val returnType = Form.SymF("num")
          val params = Form.MapF(
            Map(Form.StrF("let") -> Form.SymF("num"))
          )
          val expr = Form.NumF(5.2f)
          val args = List(returnType, params, expr)

          an [CompileError] should be thrownBy(
            Fn.analyze(f.table, f.state, args))
        }
      }
    }

    describe("the expression does not match the expected return type") {
      it("should throw a compile error") { f =>
        val returnType = Form.SymF("str")
        val params = Form.MapF(
          Map(Form.SymF("a") -> Form.SymF("num"))
        )
        val expr = Form.NumF(6.2f)
        val args = List(returnType, params, expr)

        an [CompileError] should be thrownBy(
          Fn.analyze(f.table, f.state, args))
      }
    }

    describe("the form is valid") {
      it("should return the correct ast") { f =>
        val returnType = Form.SymF("num")
        val params = Form.MapF(
          Map(Form.SymF("a") -> Form.SymF("num"))
        )
        val expr = Form.NumF(6.2f)
        val args = List(returnType, params, expr)

        Fn.analyze(f.table, f.state, args) should equal(
          Node.FlopFn(
            Type.FreeFn(List(Type.Number), Type.Number),
            List(
              (Node.SymLit(Name.LocalName("a"), Type.Number), Type.Number)
            ),
            Node.NumLit(6.0f)
          )
        )
      }
    }
  }
}
