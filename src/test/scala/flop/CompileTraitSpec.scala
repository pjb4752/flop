package flop

import org.scalatest._

import flop.analysis.CompileError

class CompileTraitSpec extends BaseCompileSpec {

  describe("compiling trait definition") {
    it("should compile") { f =>
      f.compileFn("(trait Foo {str (num num)})") should equal("")
    }

    describe("attempts to redefine existing traits") {
      describe("the existing trait is builtin") {
        it("should fail to compile") { f =>
          val flopSource = "(trait show {str (num str)})"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }

      describe("the existing trait is user-defined") {
        it("should fail to compile") { f =>
          val flopSource = "(trait show {str (num str)}) (trait show {str (str str)})"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }

    describe("non-module level defs") {
      describe("attempt to def inside a def") {
        it("should raise a compilation error") { f =>
          val flopSource = "(let (x 5) (trait Foo {str (num num)}))"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }
  }
}
