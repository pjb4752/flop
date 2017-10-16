package flop.integration

import org.scalatest._

class CompileIfSpec extends BaseCompileSpec {

  describe("compiling if special form") {
    describe("when the expressions are all literals") {
      it("should produce the correct lua") { f =>
        f.compileFn("(if (value? 1 2) 5 3)") should equal(
          """local var_1
            |if value?(1.0, 2.0) then
            |var_1 = 5.0
            |else
            |var_1 = 3.0
            |end""".stripMargin)
      }
    }

    describe("when the expressions function applications") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(if (flopcore.core.common.= testnum1 testnum2)
            |  (flopcore.core.common.+ testnum1 1)
            |  (flopcore.core.common.+ testnum2 1))""".stripMargin) should equal(
          """local var_1
            |if (testnum1 == testnum2) then
            |var_1 = (testnum1 + 1.0)
            |else
            |var_1 = (testnum2 + 1.0)
            |end""".stripMargin)
      }
    }

    describe("nested if forms") {
      it("should produce the correct lua") { f =>
        f.compileFn(
          """(if (> testnum1 testnum2)
            |  (if (> testnum1 testnum3)
            |  testnum1
            |  testnum3)
            |testnum2)""".stripMargin) should equal(
          """local var_1
            |if (testnum1 > testnum2) then
            |local var_2
            |if (testnum1 > testnum3) then
            |var_2 = testnum1
            |else
            |var_2 = testnum3
            |end
            |var_1 = var_2
            |else
            |var_1 = testnum2
            |end""".stripMargin)
      }
    }
  }
}
