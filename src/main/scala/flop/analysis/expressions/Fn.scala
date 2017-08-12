package flop.analysis.expressions

import flop.analysis.{CompileError, ModuleTree, Node, State, SymbolTable, Type}
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Fn {

  def analyze(tree: ModuleTree, state: State, args: List[Form]): Node = {
    if (args.length != 3) {
      throw CompileError("invalid fn form, expected (fn RETURN PARAM EXPR)")
    }

    val rType = SymbolTable.analyzeTypeForm(tree, args(0))
    val params = analyzeParams(tree, args(1))
    val symbols = params.map({ case (s, t) => (s.value -> t) }).toMap
    val newState = state.copy(localScopes = symbols :: state.localScopes)
    val body = newState.analyzeFn(tree, newState.copy(atTopLevel = false))(args(2))

    if (body.eType != rType) {
      throw CompileError(s"actual return type ${body.eType} does not match expected ${rType}")
    }

    val fnType = Type.FreeFn(params.map(_._2), rType)
    Node.FlopFn(fnType, params, body)
  }

  private def analyzeParams(tree: ModuleTree, form: Form): Node.Params = {
    val rawParams = form match {
      case Form.MapF(raw) => raw
      case _ => throw CompileError("fn PARAM must be a map of (NAME TYPE)")
    }

    rawParams.map({ case (n, t) => analyzeParam(tree, n, t) }).toList
  }

  private def analyzeParam(tree: ModuleTree, pName: Form, pType: Form): Node.Param = {
    val rawName = pName match {
      case Form.SymF(value) => value
      case _ => throw CompileError("fn PARAM must be a name")
    }
    if (Core.reserved.contains(rawName)) {
      throw CompileError(s"cannot redefine ${rawName}")
    }
    val symType = SymbolTable.analyzeTypeForm(tree, pType)

    (Node.SymLit(rawName, symType), symType)
  }
}
