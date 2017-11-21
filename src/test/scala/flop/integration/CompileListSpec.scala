package flop.integration

import org.scalatest._

class CompileListSpec extends BaseCompileSpec {

  describe("compiling list special form") {
    describe("creating a list from a single literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("(list/new 5)") should equal(
          "flopcore_core_list.new(5)")
      }

      it ("should handle generics") { f =>
        f.compileFn("""
          (list/head
            (list/new 5))""") should equal(
          "flopcore_core_list.head(flopcore_core_list.new(5))")
      }
    }

    describe("creating a list from multiple literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("(list/new 1 2 3)") should equal(
          "flopcore_core_list.new(1, 2, 3)")
      }
    }

    describe("creating a list from expressions") {
      it("should produce the correct lua") { f =>
        f.compileFn("""
          (list/new (+ 1 2) (+ 3 4))""") should equal(
          "flopcore_core_list.new((1 + 2), (3 + 4))")
      }
    }

    describe("creating a list of nested lists") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(list/new (list/new 4))""") should equal(
          "flopcore_core_list.new(flopcore_core_list.new(4))")
      }
    }
  }
}
