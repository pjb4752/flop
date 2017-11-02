package flop.integration

import org.scalatest._

import flop.analysis.CompileError

class CompileDefSpec extends BaseCompileSpec {

  describe("compiling def special form") {
    describe("defining from literal values") {
      it("should produce the correct lua") { f =>
        f.compileFn("(def x 5)") should equal("local x = 5")
      }
    }

    describe("defining from simple operations") {
      it("should produce the correct lua") { f =>
        f.compileFn("""
          (def x
            (flopcore.core.common.+ 1 2))"""
        ) should equal("local x = (1 + 2)")
      }
    }

    describe("defining a function") {
      it("should produce the correct lua") { f =>
        f.compileFn("""
          (def x
            (fn num {a num b num}
              (flopcore.core.common.+ a b)))""") should equal(
          """local x = function(a, b)
            |local var_1
            |var_1 = (a + b)
            |return var_1
            |end""".stripMargin)
      }
    }

    describe("attempts to redefine special values") {
      describe("when the value is a special form") {
        it("should fail to compile") { f =>
          val flopSource = "(def if 5)"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }

      describe("when the value is a builtin") {
        it("should fail to compile") { f =>
          val flopSource = "(def true 5)"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }

    describe("non-module level defs") {
      describe("attempt to def inside a def") {
        it("should raise a compilation error") { f =>
          val flopSource = "(def x (def y 5))"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }

      describe("attempt to def inside a let") {
        it("should raise a compilation error") { f =>
          val flopSource = "(let (x (def y 5)) x)"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }

      describe("attempt to def inside a nested form") {
        it("should raise a compilation error") { f =>
          val flopSource = "(if (< 2 1) (let (x 4) (def y x) x) 5)"
          an [CompileError] should be thrownBy(f.compileFn(flopSource))
        }
      }
    }
  }
}
