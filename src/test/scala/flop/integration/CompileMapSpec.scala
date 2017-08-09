package flop.integration

import org.scalatest._

class CompileMapSpec extends BaseCompileSpec {

  describe("compiling map literal") {
    describe("creating an empty map") {
      it("should produce the correct lua") { f =>
        f.compileFn("{}") should equal("Map.new()")
      }
    }

    describe("creating a map a map with simple literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("{\"one\" 1 \"two\" 2}") should equal(
            "Map.new(\"one\", 1.0, \"two\", 2.0)")
      }
    }

    describe("creating a map from expressions") {
      it("should produce the correct lua") { f =>
        f.compileFn("{(+ 1 2) (+ 3 4)}") should equal(
            "Map.new((1.0 + 2.0), (3.0 + 4.0))")
      }
    }

    describe("creating a map of nested maps") {
      it("should produce the correct lua") { f =>
        f.compileFn("{1 {2 3}}") should equal(
            "Map.new(1.0, Map.new(2.0, 3.0))")
      }
    }
  }
}
