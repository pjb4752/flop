package flop.integration

import org.scalatest._

class CompileLetSpec extends BaseCompileSpec {

  describe("compiling let special form") {
    describe("when the value is a literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x 5) x)") should equal(
          """local var_1
            |do
            |local x = 5.0
            |var_1 = x
            |end""".stripMargin)
      }
    }

    describe("when the value is a function") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x (fn num {a num b num} (flopcore.core.common.+ a b))) x)") should equal(
          """local var_1
            |do
            |local x = function(a, b)
            |local var_2
            |var_2 = (a + b)
            |return var_2
            |end
            |var_1 = x
            |end""".stripMargin)
      }
    }

    describe("when the value is the result of a function") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x (flopcore.core.common.+ 1 2)) (flopcore.core.common.+ 1 x))") should equal(
          """local var_1
            |do
            |local x = (1.0 + 2.0)
            |var_1 = (1.0 + x)
            |end""".stripMargin)
      }
    }

    describe("nested let forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(let (x 5)
            |  (let (y (flopcore.core.common.+ x 5)) y))""".stripMargin) should equal(
          """local var_1
            |do
            |local x = 5.0
            |local var_2
            |do
            |local y = (x + 5.0)
            |var_2 = y
            |end
            |var_1 = var_2
            |end""".stripMargin)
      }
    }

    describe("deeply nested let forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(let (x 5)
            |  (let (y (flopcore.core.common.+ x 5))
            |    (let (z (flopcore.core.common.+ y 3)) z)))""".stripMargin) should equal(
          """local var_1
            |do
            |local x = 5.0
            |local var_2
            |do
            |local y = (x + 5.0)
            |local var_3
            |do
            |local z = (y + 3.0)
            |var_3 = z
            |end
            |var_2 = var_3
            |end
            |var_1 = var_2
            |end""".stripMargin)
      }
    }

    describe("nested binding forms") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x (let (y 5) y)) (flopcore.core.common.+ 1 x))") should equal(
          """local var_1
            |do
            |local var_2
            |do
            |local y = 5.0
            |var_2 = y
            |end
            |local x = var_2
            |var_1 = (1.0 + x)
            |end""".stripMargin)
      }
    }

    describe("complex binding forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(let (x (if (> testnum1 testnum2) testnum1 testnum2)
            |      m 5)
            |  (flopcore.core.common.+ x m))""".stripMargin) should equal(
          """local var_1
            |do
            |local var_2
            |if (testnum1 > testnum2) then
            |var_2 = testnum1
            |else
            |var_2 = testnum2
            |end
            |local x = var_2
            |local m = 5.0
            |var_1 = (x + m)
            |end""".stripMargin)
      }
    }
  }
}
