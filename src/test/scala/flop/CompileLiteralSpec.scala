package flop

import org.scalatest._

class CompileLiteralSpec extends BaseCompileSpec {

  describe("compiling literals") {
    describe("when the literal is a number") {
      it("should produce the correct lua") { f =>
        f.compileFn("5") should equal("5.0")
      }
    }

    describe("when the literal is a string") {
      it("should produce the correct lua") { f =>
        f.compileFn("\"hi\"") should equal("\"hi\"")
      }
    }

    describe("when the literal is a symbol") {
      it("should produce the correct lua") { f =>
        f.compileFn("hi") should equal("hi")
      }
    }
  }
}
