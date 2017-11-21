package flop.integration

import org.scalatest._

class CompileSymbolSpec extends BaseCompileSpec {

  describe("compiling symbols") {
    describe("the symbol refers to a module name") {
      describe("the symbol is in the current module") {
        it("should produce the correct lua") { f =>
          f.compileFn("testnum") should equal("testnum")
        }
      }

      describe("the symbol is from another module") {
        it("should produce the correct lua") { f =>
          f.compileFn("foomod/six") should equal("core_foomod.six")
        }
      }
    }
  }
}
