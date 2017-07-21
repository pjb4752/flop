package flop

import org.scalatest._

class CompileApplySpec extends fixture.FunSpec with Matchers with Compilable {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val symbolTable = new CompilerSymbolTable()
    val compileFn = compile(symbolTable) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }

  describe("compiling function application") {
    describe("the function is infix") {
      it("should produce the correct lua") { f =>
        f.compileFn("(+ 1 2)") should equal("(1.0 + 2.0)")
      }
    }

    describe("the function is prefix") {
      it("should produce the correct lua") { f =>
        f.compileFn("(print \"hello\")") should equal("print(\"hello\")")
      }
    }

    describe("nested function calls") {
      it("should produce the correct lua") { f =>
        f.compileFn("(print (+ 1 2))") should equal("print((1.0 + 2.0))")
      }
    }
  }
}
