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

    describe("the wrong number of arguments are given") {
      it("should throw a compile error") { f =>
        val args = List[Form]()

        an [CompileError] should be thrownBy(
          Trait.analyze(f.table, f.state, args))
      }
    }
  }
}
