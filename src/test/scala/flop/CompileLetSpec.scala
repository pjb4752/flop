package flop

import org.scalatest._

class CompileLetSpec extends BaseCompileSpec {

  describe("compiling let special form") {
    describe("when the value is a literal") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x 5) x)") should equal(
          """local result_1
            |do
            |local x = 5.0
            |result_1 = x
            |end""".stripMargin)
      }
    }

    describe("when the value is the result of a function") {
      it("should produce the correct lua") { f =>
        f.compileFn("(let (x (+ 1 2)) (+ 1 x))") should equal(
          """local result_1
            |do
            |local x = (1.0 + 2.0)
            |result_1 = (1.0 + x)
            |end""".stripMargin)
      }
    }

    describe("nested let forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(let (x 5)
            |  (let (y (+ x 5)) y))""".stripMargin) should equal(
          """local result_1
            |do
            |local x = 5.0
            |local result_2
            |do
            |local y = (x + 5.0)
            |result_2 = y
            |end
            |result_1 = result_2
            |end""".stripMargin)
      }
    }

    describe("deeply nested let forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(let (x 5)
            |  (let (y (+ x 5))
            |    (let (z (+ y 3)) z)))""".stripMargin) should equal(
          """local result_1
            |do
            |local x = 5.0
            |local result_2
            |do
            |local y = (x + 5.0)
            |local result_3
            |do
            |local z = (y + 3.0)
            |result_3 = z
            |end
            |result_2 = result_3
            |end
            |result_1 = result_2
            |end""".stripMargin)
      }
    }
  }
}
