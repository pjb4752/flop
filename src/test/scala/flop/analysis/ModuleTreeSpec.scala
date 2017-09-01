package flop.analysis

import org.scalatest._

import flop.analysis.ModuleTree._

class ModuleTreeSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(tree: ModuleTree, childModule: Module)

  def withFixture(test: OneArgTest) = {
    val childModule = Module.initial("module2", "root", List("level1"))

    val tree = ModuleTree(
      "root",
      Map(
        "module1" -> Module.initial("module1", "root"),
        "level1" -> SubTree(
          "level1",
          Map(
            "module2" -> childModule,
          )
        )
      )
    )

    val fixture = FixtureParam(tree, childModule)
    withFixture(test.toNoArgTest(fixture))
  }

  describe("adding a child module") {
    describe("none of the paths exist") {
      it("return a module tree with the correct paths") { f =>
        val tree = ModuleTree.newRoot("test")
        val paths = List("path1", "path2")
        val module = Module.initial("module", "test", paths)
        val expectedTree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                "path2" -> SubTree(
                  "path2",
                  Map("module" -> module)
                )
              )
            )
          )
        )

        tree.children shouldBe empty

        val newTree = ModuleTree.addModule(tree, paths, module)

        newTree should not equal(tree)
        newTree should equal(expectedTree)
      }
    }

    describe("some of the paths exist") {
      it("should return a module tree with the correct paths") { f =>
        val tree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                "other" -> Module.initial("other", "test", List("path1"))
              )
            )
          )
        )
        val module = Module.initial("module", "test", List("path1", "path2"))
        val paths = List("path1", "path2")
        val expectedTree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                "other" -> Module.initial("other", "test", List("path1")),
                "path2" -> SubTree(
                  "path2",
                  Map("module" -> module)
                )
              )
            )
          )
        )

        val newTree = ModuleTree.addModule(tree, paths, module)

        newTree should not equal(tree)
        newTree should equal(expectedTree)
      }
    }

    describe("the path is invalid") {
      it("should throw a compile error") { f =>
        val module = Module.initial("path2", "test", List("path1"))
        val paths = List("path1", "path2")
        val tree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                "path2" -> module
              )
            )
          )
        )

        an [CompileError] should be thrownBy(
          ModuleTree.addModule(tree, paths, module))
      }
    }

    describe("the module already exists") {
      it("should throw a compile error") { f =>
        val module = Module.initial("module", "test", List("path1", "path2"))
        val paths = List("path1", "path2")
        val tree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                "other" -> Module.initial("other", "test", List("path1")),
                "path2" -> SubTree(
                  "path2",
                  Map("module" -> module)
                )
              )
            )
          )
        )

        an [CompileError] should be thrownBy(
          ModuleTree.addModule(tree, paths, module))
      }
    }
  }

  describe("validating a module path") {
    describe("when the path is valid") {
      it("should return true") { f =>
        val paths = List("level1", "module2")
        ModuleTree.isValidPath(f.tree, paths) shouldBe true
      }
    }

    describe("when the path is invalid") {
      it("should return false") { f =>
        val paths = List("level1", "module1")
        ModuleTree.isValidPath(f.tree, paths) shouldBe false
      }
    }
  }

  describe("finding a child module") {
    describe("when the module exists") {
      it("should return the child module") { f =>
        val paths = List("level1", "module2")
        ModuleTree.findModule(f.tree, paths) should equal(Some(f.childModule))
      }
    }

    describe("when the module does not exist") {
      it("should return None") { f =>
        val paths = List("level1", "module1")
        ModuleTree.findModule(f.tree, paths) should equal(None)
      }
    }
  }
}
