package flop

import org.scalatest._

class CompileDefSpec extends fixture.FunSpec with Matchers with Compilable {

  case class FixtureParam(compileFn: String => String)

  def withFixture(test: OneArgTest) = {
    val symbolTable = new CompilerSymbolTable()
    val compileFn = compile(symbolTable) _
    val fixture = FixtureParam(compileFn)

    withFixture(test.toNoArgTest(fixture))
  }

  describe("compiling def special form") {
    describe("defining from literal values") {
      it("should produce the correct lua") { f =>
        f.compileFn("(def x 5)") should equal("local x = 5.0")
      }
    }

    describe("defining from simple operations") {
      it("should produce the correct lua") { f =>
        f.compileFn("(def x (+ 1 2))") should equal("local x = (1.0 + 2.0)")
      }
    }

    describe("non-module level defs") {
      describe("attempt to def inside a def") {
        it("should raise a compilation error") { f =>
          val flopSource = "(def x (def y 5))"
          an [Analyze.CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }

      describe("attempt to def inside a let") {
        it("should raise a compilation error") { f =>
          val flopSource = "(let (x (def y 5)) x)"
          an [Analyze.CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }

      describe("attempt to def inside a nested form") {
        it("should raise a compilation error") { f =>
          val flopSource = "(if (< 2 1) (let (x 4) (def y x) x) 5)"
          an [Analyze.CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }
  }
}
