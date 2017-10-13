package flop.analysis.expressions

import org.scalatest._

import flop.analysis.{CompileError, Name, Node, Type}
import flop.reading.Form

class TraitSpec extends BaseAnalysisSpec {

  describe("analyzing a trait form") {
    describe("the form is not at the top level") {
      it("should raise a compile error") { f =>
        val name = Form.SymF("Trait")
        val fnDefName = Form.SymF("Fn")
        val fnDefFn = Form.ListF(
          List(
            Form.SymF("num"),
            Form.MapF(
              Map(Form.SymF("a") -> Form.SymF("num"))
            )
          )
        )
        val fnDefsForm = Form.MapF(Map(fnDefName -> fnDefFn))
        val args = List(name, fnDefsForm)
        val newState = f.state.copy(atTopLevel = false)

        an [CompileError] should be thrownBy(
          Trait.analyze(f.table, newState, args))
      }
    }

    describe("there is a syntax error") {
      it("should throw a compile error") { f =>
        val args = List[Form]()

        an [CompileError] should be thrownBy(
          Trait.analyze(f.table, f.state, args))
      }
    }

    describe("the name is not a symbol") {
      it("should throw a compile error") { f =>
        val name = Form.StrF("Trait")
        val fnDefName = Form.SymF("Fn")
        val fnDefFn = Form.ListF(
          List(
            Form.SymF("num"),
            Form.MapF(
              Map(Form.SymF("a") -> Form.SymF("num"))
            )
          )
        )
        val fnDefsForm = Form.MapF(Map(fnDefName -> fnDefFn))
        val args = List(name, fnDefsForm)

        an [CompileError] should be thrownBy(
          Trait.analyze(f.table, f.state, args))
      }
    }

    describe("the name is qualified") {
      it("should throw a compile error") { f =>
        val name = Form.SymF("foo.Trait")
        val fnDefName = Form.SymF("Fn")
        val fnDefFn = Form.ListF(
          List(
            Form.SymF("num"),
            Form.MapF(
              Map(Form.SymF("a") -> Form.SymF("num"))
            )
          )
        )
        val fnDefsForm = Form.MapF(Map(fnDefName -> fnDefFn))
        val args = List(name, fnDefsForm)

        an [CompileError] should be thrownBy(
          Trait.analyze(f.table, f.state, args))
      }
    }

    describe("the name is a reserved name") {
      it("should throw a compile error") { f =>
        val name = Form.SymF("def")
        val fnDefName = Form.SymF("Fn")
        val fnDefFn = Form.ListF(
          List(
            Form.SymF("num"),
            Form.MapF(
              Map(Form.SymF("a") -> Form.SymF("num"))
            )
          )
        )
        val fnDefsForm = Form.MapF(Map(fnDefName -> fnDefFn))
        val args = List(name, fnDefsForm)

        an [CompileError] should be thrownBy(
          Trait.analyze(f.table, f.state, args))
      }
    }

    describe("the form is valid") {
      it("should return the proper ast for the trait") { f =>
        val name = Form.SymF("Trait")
        val fnDefName = Form.SymF("Fn")
        val fnDefFn = Form.ListF(
          List(
            Form.SymF("num"),
            Form.ListF(
              List(Form.SymF("self"))
            )
          )
        )
        val fnDefsForm = Form.MapF(Map(fnDefName -> fnDefFn))
        val args = List(name, fnDefsForm)

        val ast = Node.TraitN(
          Node.SymLit(
            Name.ModuleName("user", List("core", "testm"), "Trait"),
            Type.Trait
          ),
          Map(
            Node.SymLit(
              Name.TraitFnName(
                Name.ModuleName("user", List("core", "testm"), "Trait"),
                "Fn"
              ),
              Type.TraitFn(List(Type.Self), Type.Number)
            ) ->
            Node.FnDef(
              Node.SymLit(
                Name.ModuleName("user", List("core", "testm"), "Trait"),
                Type.Trait
              ),
              Node.SymLit(
                Name.TraitFnName(
                  Name.ModuleName("user", List("core", "testm"), "Trait"),
                  "Fn"
                ),
                Type.TraitFn(List(Type.Self), Type.Number)
              ),
              Type.TraitFn(List(Type.Self), Type.Number)
            )
          )
        )

        Trait.analyze(f.table, f.state, args) should equal(ast)
      }
    }
  }
}
