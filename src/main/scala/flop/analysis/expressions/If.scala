package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object If {

  case object SyntaxError extends flop.analysis.Error {
    val message = """if expressions must be of the form:
                    |  (if TEST IF-EXPR ELSE-EXPR)""".stripMargin
  }

  case class TestTypeError(t: Type) extends flop.analysis.Error {
    val message = s"""if TEST expr must evaluate to a Boolean result
                      |  found: ${t}""".stripMargin
  }

  case class IfTypeError(i: Type, e: Type) extends flop.analysis.Error {
    val message = s"""if IF-EXPR and ELSE-EXPR evaluate to different types
                      | IF-EXPR type: ${i}
                      | ELSE-EXPR type: ${e}""".stripMargin
  }

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError(SyntaxError)
    } else {
      val newState = state.copy(atTopLevel = false)
      val mapFn = newState.analyzeFn(tree, newState)

      val analyzed = args.map(mapFn)
      val testType = analyzed(0).eType
      val ifType = analyzed(1).eType
      val elseType = analyzed(2).eType

      if (testType != Type.Boolean) {
        throw CompileError(TestTypeError(testType))
      } else if (ifType != elseType) {
        throw CompileError(IfTypeError(ifType, elseType))
      }

      Node.IfN(analyzed(0), analyzed(1), analyzed(2), ifType)
    }
  }
}
