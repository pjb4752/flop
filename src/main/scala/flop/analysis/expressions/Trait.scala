package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, SymbolTable, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object Trait {

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 2) {
      throw CompileError("invalid trait form, expected (trait NAME FNDEFS")
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      throw CompileError("trait expects first arg to be a name")
    } else if (!state.atTopLevel) {
      throw CompileError("trait must occur at module (top) level")
    } else {
      val symbolText = args.head.asInstanceOf[Form.SymF].value

      // TODO check for trait definition
      //if (state.currentModule.traits.contains(symbolText)) {
        //throw CompileError(s"trait ${symbolText} already defined in this module")
      //}
      val symbol = Node.SymLit(symbolText, Type.Trait)
      val fnDefs = analyzeFnDefs(tree, symbol, args(1))

      Node.TraitN(symbol, fnDefs)
    }
  }

  private def analyzeFnDefs(tree: ModuleTree, traitName: Node.SymLit, form: Form): Map[Node.SymLit, Node.FnDef] = {
    val rawDefs = form match {
      case Form.MapF(raw) => raw
      case _ => throw CompileError("FNDEFS must be a map of (NAME FNDEF)")
    }

    rawDefs.map({ case (n, f) => analyzeFnDef(tree, traitName, n, f) }).toMap
  }

  private def analyzeFnDef(tree: ModuleTree, traitName: Node.SymLit, fName: Form, fnDef: Form): (Node.SymLit, Node.FnDef) = {
    val rawName = fName match {
      case Form.SymF(value) => value
      case _ => throw CompileError("FNDEF must be a (NAME DEF) pair")
    }
    val rawFnDef = fnDef match {
      case Form.ListF(values) => values
      case _ => throw CompileError("FNDEF must be a (NAME DEF) pair")
    }
    if (rawFnDef.length < 1) {
      throw CompileError("invalid FNDEF form, expected (RETURN [PARAMS])")
    }

    val types = rawFnDef.map(t => SymbolTable.analyzeTypeForm(tree, t))
    val fnType = Type.TraitFn(types.tail, types.head)
    val fnName = Node.SymLit(rawName, fnType)

    (fnName, Node.FnDef(traitName, fnName, fnType))
  }
}
