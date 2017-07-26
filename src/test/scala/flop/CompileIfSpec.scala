package flop

import org.scalatest._

class CompileIfSpec extends BaseCompileSpec {

  describe("compiling if special form") {
    describe("when the expressions are all literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("(if value? 5 3)") should equal(
          """local var_1
            |if value? then
            |var_1 = 5.0
            |else
            |var_1 = 3.0
            |end""".stripMargin)
      }
    }

    describe("when the expressions function applications") {
      it("should produce the correct lua") { f =>
        f.compileFn("(if (= x y) (+ x 1) (+ y 1))") should equal(
          """local var_1
            |if (x == y) then
            |var_1 = (x + 1.0)
            |else
            |var_1 = (y + 1.0)
            |end""".stripMargin)
      }
    }

    describe("nested if forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(if (> x y)
            |  (if (> x z)
            |  x
            |  z)
            |y)""".stripMargin) should equal(
          """local var_1
            |if (x > y) then
            |local var_2
            |if (x > z) then
            |var_2 = x
            |else
            |var_2 = z
            |end
            |var_1 = var_2
            |else
            |var_1 = y
            |end""".stripMargin)
      }
    }
  }
}
