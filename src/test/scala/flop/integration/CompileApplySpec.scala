package flop.integration

import org.scalatest._

class CompileApplySpec extends BaseCompileSpec {

  describe("compiling function application") {
    describe("the function is infix") {
      it("should produce the correct lua") { f =>
        f.compileFn("(+ 1 2)") should equal("(1 + 2)")
      }
    }

    describe("the function is prefix") {
      it("should produce the correct lua") { f =>
        f.compileFn("(print \"hello\")") should equal("print(\"hello\")")
      }
    }

    describe("nested function calls") {
      it("should produce the correct lua") { f =>
        f.compileFn("(+ 4 (+ 1 2))") should equal("(4 + (1 + 2))")
      }
    }
  }
}
