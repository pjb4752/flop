package flop.integration

import org.scalatest._

import flop.analysis.CompileError

class CompileFnSpec extends BaseCompileSpec {

  describe("compiling fn special form") {
    describe("compilation of simple functions") {
      it("should produce the correct lua") { f =>
        f.compileFn("(fn num {a num b num} (flopcore.core.common.+ a b))") should equal(
          """function(a, b)
            |local var_1
            |var_1 = (a + b)
            |return var_1
            |end""".stripMargin)
      }
    }

    describe("compilation of functions with conditional statements") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(fn str {a num b num}
            |  (if (> a b)
            |    "max is a"
            |    "max is b"))""".stripMargin) should equal(
          """function(a, b)
            |local var_1
            |local var_2
            |if (a > b) then
            |var_2 = "max is a"
            |else
            |var_2 = "max is b"
            |end
            |var_1 = var_2
            |return var_1
            |end""".stripMargin)
      }
    }
  }

  describe("compilation of functions with let forms") {
    it ("should produce the correct lua") { f =>
        f.compileFn(
          """(fn num {a num}
            |  (let (b 5)
            |    (flopcore.core.common.+ a b)))""".stripMargin) should equal(
          """function(a)
            |local var_1
            |local var_2
            |do
            |local b = 5.0
            |var_2 = (a + b)
            |end
            |var_1 = var_2
            |return var_1
            |end""".stripMargin)
    }
  }

  describe("attempt to compile functions with reserved params") {
    describe("the reserved name is a special form") {
      it("should fail to compile") { f =>
        val flopSource = "(fn num {x num def num} (+ x def))"
        an [CompileError] should be thrownBy(f.compileFn(flopSource))
      }
    }

    describe("the reserved name is a builtin") {
      it("should fail to compile") { f =>
        val flopSource = "(fn num {x num true num} (+ x true))"
        an [CompileError] should be thrownBy(f.compileFn(flopSource))
      }
    }
  }
}
