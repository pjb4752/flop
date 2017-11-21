package flop.integration

import org.scalatest._

class CompileMapSpec extends BaseCompileSpec {

  describe("compiling map literal") {
    describe("creating a map a map with simple literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("{\"one\" 1 \"two\" 2}") should equal(
            "flopcore_core_map.new(flopcore_core_pair.new(\"one\", 1), flopcore_core_pair.new(\"two\", 2))")
      }
    }

    describe("creating a map from expressions") {
      it("should produce the correct lua") { f =>
        f.compileFn("""
          {
            (+ 1 2) (+ 3 4)
          }""") should equal(
            "flopcore_core_map.new(flopcore_core_pair.new((1 + 2), (3 + 4)))")
      }
    }

    describe("creating a map of nested maps") {
      it("should produce the correct lua") { f =>
        f.compileFn("{1 {2 3}}") should equal(
            "flopcore_core_map.new(flopcore_core_pair.new(1, flopcore_core_map.new(flopcore_core_pair.new(2, 3))))")
      }
    }

    describe("getting the value of a key") {
      it("should produce the correct lua") { f =>
        f.compileFn("(map/get {\"hi\" 5} \"hi\")") should equal(
          "flopcore_core_map.get(flopcore_core_map.new(flopcore_core_pair.new(\"hi\", 5)), \"hi\")")
      }
    }
  }
}
