package flop.integration

import org.scalatest._

import flop.analysis.CompileError

class CompileTraitSpec extends BaseCompileSpec {

  describe("compiling trait definition") {
    it("should compile") { f =>
      f.compileFn("(trait Foo {tostring (str (self))})") should equal("")
    }

    describe("attempts to redefine existing traits") {
      describe("the existing trait is user-defined") {
        it("should fail to compile") { f =>
          val flopSource = """(trait Show {tostring (str (self))})
                              (trait Show {tostring (str (self))})"""
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }

    describe("non-module level defs") {
      describe("attempt to def a trait inside another form") {
        it("should raise a compilation error") { f =>
          val flopSource = "(let (x 5) (trait Foo {tostring (str (self))}))"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }
  }
}
