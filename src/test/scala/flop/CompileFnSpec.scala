package flop

import org.scalatest._

class CompileFnSpec extends BaseCompileSpec {

  describe("compiling fn special form") {
    describe("compilation of simple functions") {
      it("should produce the correct lua") { f =>
        f.compileFn("(fn (a b) (+ a b))") should equal(
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
          """(fn (a b)
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
          """(fn (a)
            |  (let (b 5)
            |    (+ a b)))""".stripMargin) should equal(
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
}
