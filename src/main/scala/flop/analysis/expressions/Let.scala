package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Let {

  type FlopError = flop.analysis.Error

  case object SyntaxError extends FlopError {
    val message = """let expressions must be of the form:
                    |  (let BIND EXPR)""".stripMargin
  }

  case object BindSyntaxError extends FlopError {
    val message = """let BIND expressions must be of the form:
                    |  (NAME EXPR NAME EXPR)""".stripMargin
  }

  case object BindTermError extends FlopError {
    val message = "let BIND expressions have an even number of terms"
  }

  case object BindNameError extends FlopError {
    val message = "let BIND expects first value to be a name"
  }

  case class RedefineError(name: String) extends FlopError {
    val message = s"def error: cannot redefine var ${name}"
  }

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError(SyntaxError)
    } else {
      val bindings = analyzeBindings(tree, state, args(0))
      val symbols = bindings.map({ case (s, e) => (s.name.name -> e.eType) }).toMap
      val newState = state.copy(localScopes = symbols :: state.localScopes)
      val expr = newState.analyzeFn(tree, newState.copy(atTopLevel = false))(args(1))

      Node.LetN(bindings, expr, expr.eType)
    }
  }

  private def analyzeBindings(tree: ModuleTree, state: State, form: Form): List[(Node.SymLit, Node)] = {
    val rawBindings = form match {
      case Form.ListF(raw) => raw
      case _ => throw CompileError(BindSyntaxError)
    }
    if (rawBindings.length % 2 != 0) {
      throw CompileError(BindTermError)
    }
    rawBindings.grouped(2).map(analyzeBinding(tree, state)).toList
  }

  private def analyzeBinding(tree: ModuleTree, state: State)(forms: List[Form]): (Node.SymLit, Node) = {
    if (!forms.head.isInstanceOf[Form.SymF]) {
      throw CompileError(BindNameError)
    } else {
      val symbolText = forms(0).asInstanceOf[Form.SymF].value
      val expr = state.analyzeFn(tree, state.copy(atTopLevel = false))(forms(1))

      if (SymbolTable.isReservedName(symbolText)) {
        throw CompileError(RedefineError(symbolText))
      }
      val localName = LocalName(symbolText)
      val symbol = Node.SymLit(localName, expr.eType)

      (symbol, expr)
    }
  }
}
