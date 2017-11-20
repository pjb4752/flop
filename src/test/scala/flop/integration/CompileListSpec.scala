package flop.integration

import org.scalatest._

class CompileListSpec extends BaseCompileSpec {

  describe("compiling list special form") {
    describe("creating a list from a single literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("(flopcore.core.list.new 5)") should equal(
          "flopcore_core_list.new(5)")
      }

      it ("should handle generics") { f =>
        f.compileFn("""
          (flopcore.core.list.head
            (flopcore.core.list.new 5))""") should equal(
          "flopcore_core_list.head(flopcore_core_list.new(5))")
      }
    }

    describe("creating a list from multiple literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("(flopcore.core.list.new 1 2 3)") should equal(
          "flopcore_core_list.new(1, 2, 3)")
      }
    }

    describe("creating a list from expressions") {
      it("should produce the correct lua") { f =>
        f.compileFn("""
          (flopcore.core.list.new
            (+ 1 2)
            (+ 3 4))""") should equal(
          "flopcore_core_list.new((1 + 2), (3 + 4))")
      }
    }

    describe("creating a list of nested lists") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(flopcore.core.list.new
            (flopcore.core.list.new 4))""") should equal(
          "flopcore_core_list.new(flopcore_core_list.new(4))")
      }
    }
  }
}
