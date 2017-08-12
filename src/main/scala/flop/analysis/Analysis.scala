package flop.analysis

import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

import flop.analysis.expressions._

object Analysis {

  def analyze(tree: ModuleTree, forms: List[Form]): List[Node] = {
    val state = State(true, "user", tryAnalyze)
    forms.map(tryAnalyze(tree, state))
  }

  private def tryAnalyze(tree: ModuleTree, state: State)(form: Form): Node = form match {
    case Form.NumF(v) => Node.NumLit(v)
    case Form.StrF(v) => Node.StrLit(v)
    case Form.SymF(v) => analyzeSymbol(tree, state, v)
    case Form.ListF(l) => analyzeList(tree, state, l)
    case Form.MapF(m) => analyzeMap(tree, state, m)
  }

  private def analyzeSymbol(tree: ModuleTree, state: State, name: String): Node = {
    if (Core.specialForms.contains(name)) {
      throw CompileError(s"cannot take value of special form ${name}")
    }

    name match {
      case "true" => Node.TrueLit
      case "false" => Node.FalseLit
      case _ => {
        val eType = SymbolTable.lookupType(tree, state, name)
        Node.SymLit(name, eType)
      }
    }
  }

  private def analyzeList(tree: ModuleTree, state: State, list: List[Form]): Node = list match {
    case op :: args => analyzeOp(tree, state, op, args)
    case _ => Node.ListLit(List[Node]())
  }

  private def analyzeMap(tree: ModuleTree, state: State, map: Map[Form, Form]): Node = {
    val analyzeFn = tryAnalyze(tree, state.copy(atTopLevel = false)) _
    Node.MapLit(map.map({ case (k ,v) => (analyzeFn(k), analyzeFn(v)) }))
  }

  private def analyzeOp(tree: ModuleTree, state: State, op: Form, args: List[Form]): Node = op match {
    case Form.SymF(s) => s match {
      case "def" => Def.analyze(tree, state, args)
      case "let" => Let.analyze(tree, state, args)
      case "if" => If.analyze(tree, state, args)
      case "fn" => Fn.analyze(tree, state, args)
      case "trait" => Trait.analyze(tree, state, args)
      case "list" => ListExpr.analyze(tree, state, args)
      case _ => Apply.analyze(tree, state, s, args)
    }
    case u => throw CompileError(s"cannot apply ${u}")
  }
}
