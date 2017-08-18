package flop.analysis

import org.scalatest._

class ModuleTreeSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(tree: ModuleTree)

  def withFixture(test: OneArgTest) = {
    val tree = ModuleTree(
      "root",
      Map[String, MNode](
        "module1" -> SubModule("module1", Module.initial("module1")),
        "level1" -> ModuleTree(
          "level1",
          Map[String, MNode](
            "module2" -> SubModule("module2", Module.initial("module2")),
          )
        )
      )
    )

    val fixture = FixtureParam(tree)
    withFixture(test.toNoArgTest(fixture))
  }

  describe("adding a child module") {
    it("return a new module with child added") { f =>
      val tree = ModuleTree.newRoot("test")
      val module = Module.initial("module")

      tree.children shouldBe empty

      val result = ModuleTree.addChildModule(tree, module)
      val subModule = result.children("module").asInstanceOf[SubModule]

      subModule.name should equal("module")
      subModule.value should equal(module)
    }
  }

  describe("validating a module path") {
    describe("when the path is valid") {
      it("should return true") { f =>
        val paths = List("root", "level1", "module2")
        ModuleTree.validatePath(f.tree, paths) shouldBe true
      }
    }

    describe("when the path is invalid") {
      it("should return false") { f =>
        val paths = List("root", "level1", "module1")
        ModuleTree.validatePath(f.tree, paths) shouldBe false
      }
    }
  }
}