package flop

import org.scalatest._

class CompileListSpec extends BaseCompileSpec {

  describe("compiling list special form") {
    describe("creating a list from a single literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("(list a)") should equal("List.new(a)")
      }
    }

    describe("creating a list from multiple literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("(list 1 2 3)") should equal("List.new(1.0, 2.0, 3.0)")
      }
    }

    describe("creating a list from expressions") {
      it("should produce the correct lua") { f =>
        f.compileFn("(list (+ 1 2) (+ 3 4))") should equal(
          "List.new((1.0 + 2.0), (3.0 + 4.0))")
      }
    }

    describe("creating a list of nested lists") {
      it("should produce the correct lua") { f =>
        f.compileFn("(list (list 4))") should equal("List.new(List.new(4.0))")
      }
    }
  }
}
