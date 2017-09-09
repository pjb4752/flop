package flop.io

import java.nio.file.{Path, Paths}
import org.scalatest._

import flop.analysis.Name

class ModuleIOSpec extends FunSpec with Matchers {

  describe("finding the path of a module") {
    it("should return the correct path") {
      val mName = Name.ModuleName("root", List("thing"), "other")
      val expected = Paths.get("root/thing/other.flp")

      ModuleIO.pathFromName(mName) should equal(expected)
    }
  }
}
