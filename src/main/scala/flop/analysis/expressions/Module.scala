package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object Module {

  object Error {
    case object NestedDefinitionError extends Error {
      val message = "module definitions must occur at the top level"
    }

    case object BadArgumentsError extends Error {
      val message = """module definitions must be of the form:
                      |  (module NAME [IMPORTS])""".stripMargin
    }

    case object BadNameError extends Error {
      val message = "module NAME must be a symbol"
    }
  }

  //def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    //if (!state.atTopLevel) {
      //throw CompileError(Error.NestedDefinitionError.message)
    //} else if (args.length < 1) {
      //throw CompileError(Error.BadArgumentsError.message))
    //}

    //val moduleName = analyzeModuleName(state: State, args.head)
  //}

  //private def analyzeModuleName(state: State, form: Form) =
    //val nameLit = form match {
      //case f: Form.Sym => f.value
      //case _ => throw CompileError(Error.BadNameError.message)
    //}

    //val nameParts = nameLit.split("\\.")
    //ModuleTree.validate(nameParts)

  //}
}
