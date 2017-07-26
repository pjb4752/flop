package flop

import org.scalatest._

class CompileIfSpec extends BaseCompileSpec {

  describe("compiling if special form") {
    describe("when the expressions are all literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("(if value? 5 3)") should equal(
          """local result_1
            |if value? then
            |result_1 = 5.0
            |else
            |result_1 = 3.0
            |end""".stripMargin)
      }
    }

    describe("when the expressions function applications") {
      it("should produce the correct lua") { f =>
        f.compileFn("(if (= x y) (+ x 1) (+ y 1))") should equal(
          """local result_1
            |if (x == y) then
            |result_1 = (x + 1.0)
            |else
            |result_1 = (y + 1.0)
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
          """local result_1
            |if (x > y) then
            |local result_2
            |if (x > z) then
            |result_2 = x
            |else
            |result_2 = z
            |end
            |result_1 = result_2
            |else
            |result_1 = y
            |end""".stripMargin)
      }
    }
  }
}
