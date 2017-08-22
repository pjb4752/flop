package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object Trait {

  type FlopError = flop.analysis.Error

  case object NestedDefinitionError extends FlopError {
    val message = "trait definitions must occur at the top level"
  }

  case object SyntaxError extends FlopError {
    val message = """trait expressions must be of the form:
                    |  (trait NAME FNDEFS)""".stripMargin
  }

  case object BadNameError extends FlopError {
    val message = "trait NAME must be a symbol"
  }

  case object FnDefsError extends FlopError {
    val message = """trait FNDEFS must be of the form:
                    |  {NAME FNDEF NAME FNDEF}""".stripMargin
  }

  case object FnDefError extends FlopError {
    val message = """trait FNDEF must be of the form:
                    |  (RETURN-TYPE {PARAMS})""".stripMargin
  }

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    if (!state.atTopLevel) {
      throw CompileError(NestedDefinitionError)
    } else if (args.length != 2) {
      throw CompileError(SyntaxError)
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw CompileError(BadNameError)
    } else {
      // TODO check if symbol contains('.')
      val symbolText = args.head.asInstanceOf[Form.SymF].value

      // TODO check for trait definition
      //if (state.currentModule.traits.contains(symbolText)) {
        //throw CompileError(s"trait ${symbolText} already defined in this module")
      //}
      val name = ModuleName(state.currentTree, state.currentPaths, symbolText)
      val symbol = Node.SymLit(name, Type.Trait)
      val fnDefs = analyzeFnDefs(table, state, symbol, args(1))

      Node.TraitN(symbol, fnDefs)
    }
  }

  private def analyzeFnDefs(table: SymbolTable, state: State, traitName: Node.SymLit, form: Form): Map[Node.SymLit, Node.FnDef] = {
    val rawDefs = form match {
      case Form.MapF(raw) => raw
      case _ => throw CompileError(FnDefsError)
    }

    rawDefs.map({ case (n, f) => analyzeFnDef(table, state, traitName, n, f) }).toMap
  }

  private def analyzeFnDef(table: SymbolTable, state: State, traitName: Node.SymLit, fName: Form, fnDef: Form): (Node.SymLit, Node.FnDef) = {
    val rawName = fName match {
      case Form.SymF(value) => value
      case _ => throw CompileError(FnDefsError)
    }
    val rawFnDef = fnDef match {
      case Form.ListF(values) => values
      case _ => throw CompileError(FnDefsError)
    }
    if (rawFnDef.length < 1) {
      throw CompileError(FnDefError)
    }

    val types = rawFnDef.map(t => SymbolTable.analyzeTypeForm(table, t))
    val fnType = Type.TraitFn(types.tail, types.head)
    val name = ModuleName(state.currentTree, state.currentPaths, rawName)
    val fnName = Node.SymLit(name, fnType)

    (fnName, Node.FnDef(traitName, fnName, fnType))
  }
}
