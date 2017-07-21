package flop

import org.scalatest._

class CompileLetSpec extends fixture.FunSpec with Matchers with Compilable {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val symbolTable = new CompilerSymbolTable()
    val compileFn = compile(symbolTable) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }

  describe("compiling let special form") {
    describe("when the value is a literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x 5) x)") should equal(
          """local result_1
            |do
            |  local x = 5.0
            |  result_1 = x
            |end""".stripMargin)
      }
    }

    describe("when the value is the result of a function") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x (+ 1 2)) (+ 1 x))") should equal(
          """local result_1
            |do
            |  local x = (1.0 + 2.0)
            |  result_1 = (1.0 + x)
            |end""".stripMargin)
      }
    }
  }
}
