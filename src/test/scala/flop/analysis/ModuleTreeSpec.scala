package flop.analysis

import org.scalatest._

import flop.analysis._
import flop.analysis.ModuleTree._

class ModuleTreeSpec extends fixture.FunSpec with Matchers {

  case class FixtureParam(tree: ModuleTree, childModule: Module)

  def withFixture(test: OneArgTest) = {
    val moduleName = Name.ModuleName("root", List[String](), "module1")
    val childName = Name.ModuleName("root", List("level1"), "module2")
    val childModule = Module.initial(childName)

    val tree = ModuleTree(
      "root",
      Map(
        moduleName.name -> Module.initial(moduleName),
        "level1" -> SubTree(
          "level1",
          Map(
            childName.name -> childModule,
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
        val moduleName = Name.ModuleName("test", paths, "module")
        val module = Module.initial(moduleName)
        val expectedTree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                "path2" -> SubTree(
                  "path2",
                  Map(moduleName.name -> module)
                )
              )
            )
          )
        )

        tree.children shouldBe empty

        val newTree = ModuleTree.addModule(tree, module)

        newTree should not equal(tree)
        newTree should equal(expectedTree)
      }
    }

    describe("some of the paths exist") {
      it("should return a module tree with the correct paths") { f =>
        val m1Name = Name.ModuleName("test", List("path1"), "other")
        val tree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                m1Name.name -> Module.initial(m1Name)
              )
            )
          )
        )
        val m2Name = Name.ModuleName("test", List("path1", "path2"), "module")
        val module = Module.initial(m2Name)
        val expectedTree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                m1Name.name -> Module.initial(m1Name),
                "path2" -> SubTree(
                  "path2",
                  Map(m2Name.name -> module)
                )
              )
            )
          )
        )

        val newTree = ModuleTree.addModule(tree, module)

        newTree should not equal(tree)
        newTree should equal(expectedTree)
      }
    }

    describe("the path is invalid") {
      it("should throw a compile error") { f =>
        val mName = Name.ModuleName("test", List("path1"), "path2")
        val module = Module.initial(mName)
        val tree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                mName.name -> module
              )
            )
          )
        )

        an [CompileError] should be thrownBy(
          ModuleTree.addModule(tree, module))
      }
    }

    describe("the module already exists") {
      it("should throw a compile error") { f =>
        val otherName = Name.ModuleName("test", List("path1"), "other")
        val mName = Name.ModuleName("test", List("path1", "path2"), "module")
        val module = Module.initial(mName)
        val tree = ModuleTree(
          "test",
          Map(
            "path1" -> SubTree(
              "path1",
              Map(
                otherName.name -> Module.initial(otherName),
                "path2" -> SubTree(
                  "path2",
                  Map(mName.name -> module)
                )
              )
            )
          )
        )

        an [CompileError] should be thrownBy(
          ModuleTree.addModule(tree, module))
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
