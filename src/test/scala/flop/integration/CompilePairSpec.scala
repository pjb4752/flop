package flop.integration

import org.scalatest._

class CompilePairSpec extends BaseCompileSpec {

  describe("compiling pair literal") {
    it("should produce the correct lua") { f =>
      f.compileFn("#[5 true]") should equal("flopcore_core_pair.new(5, true)")
    }
  }

  describe("getting the first pair value") {
    it("should produce the correct lua") { f =>
      f.compileFn("""
        (flopcore.core.common.+
          (flopcore.core.pair.first #[5 true])
          10)""") should equal(
          "(flopcore_core_pair.first(flopcore_core_pair.new(5, true)) + 10)"
        )
    }
  }

  describe("getting the last pair value") {
    it("should produce the correct lua") { f =>
      f.compileFn("""
        (flopcore.core.common.+
          (flopcore.core.pair.last #[false 7])
          10)""") should equal(
          "(flopcore_core_pair.last(flopcore_core_pair.new(false, 7)) + 10)"
        )
    }
  }
}
