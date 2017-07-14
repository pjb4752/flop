package flop

import org.scalatest._

class MainSpect extends FlatSpec with Matchers {
  "The fake test" should "be true" in {
    1 shouldEqual 1
  }
}
