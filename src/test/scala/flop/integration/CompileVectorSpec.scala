package flop.integration

import org.scalatest._

class CompileVectorSpec extends BaseCompileSpec {

  describe("compiling vector literal") {
    describe("creating a vector from a single literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("[5]") should equal("flopcore_core_vector.new(5)")
      }
    }

    describe("creating a vector from multiple literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("[1 2 3]") should equal(
          "flopcore_core_vector.new(1, 2, 3)")
      }
    }

    describe("creating a vector from expressions") {
      it("should produce the correct lua") { f =>
        f.compileFn("[(+ 1 2) (+ 3 4)]") should equal(
          "flopcore_core_vector.new((1 + 2), (3 + 4))")
      }
    }

    describe("creating a vector of vectors") {
      it("should produce the correct lua") { f =>
        f.compileFn("[[4]]") should equal(
          "flopcore_core_vector.new(flopcore_core_vector.new(4))")
      }
    }

    describe("getting the element of a vector") {
      it("should produce the correct lua") { f =>
        f.compileFn("(flopcore.core.vector.get [5 4 3] 1)") should equal(
          "flopcore_core_vector.get(flopcore_core_vector.new(5, 4, 3), 1)")
      }
    }
  }
}
