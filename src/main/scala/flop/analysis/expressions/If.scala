package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object If {

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError("invalid if form, expected (if TEST IF-EXPR ELSE-EXPR)")
    } else {

      val mapFn = state.analyzeFn(tree, state.copy(atTopLevel = false))
      val analyzed = args.map(mapFn)
      val testType = analyzed(0).eType
      val ifType = analyzed(1).eType
      val elseType = analyzed(2).eType

      if (testType != Type.Boolean) {
        throw CompileError(s"if test type ${testType} is not a Boolean")
      } else if (ifType != elseType) {
        throw CompileError(s"if expr type ${ifType} does not match else expr type ${elseType}")
      }

      Node.IfN(analyzed(0), analyzed(1), analyzed(2), ifType)
    }
  }
}
