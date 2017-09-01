package flop.analysis

import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

import flop.analysis.expressions._

object Analysis {

  def analyzeModule(table: SymbolTable, forms: List[Form]): List[Node] = {
    val module = Module.analyze(table, forms.head)

    List[Node]()
  }

  def analyze(table: SymbolTable, forms: List[Form]): List[Node] = {
    val state = State(tryAnalyze, true, "user", List("default"))

    // modify SymbolTable here by adding Vars and Traits
    forms.map(tryAnalyze(table, state))
  }

  private def tryAnalyze(table: SymbolTable, state: State)(form: Form): Node = form match {
    case Form.NumF(v) => Node.NumLit(v)
    case Form.StrF(v) => Node.StrLit(v)
    case Form.SymF(v) => analyzeSymbol(table, state, v)
    case Form.ListF(l) => analyzeList(table, state, l)
    case Form.MapF(m) => analyzeMap(table, state, m)
  }

  private def analyzeSymbol(table: SymbolTable, state: State, raw: String): Node = {
    val maybeName = SymbolTable.lookupName(table, state, raw)

    if (maybeName.isEmpty) {
      throw CompileError.undefinedError(raw)
    } else {
      val name = maybeName.get
      val eType = SymbolTable.lookupType(table, state, name)

      Node.SymLit(name, eType)
    }
  }

  private def analyzeList(table: SymbolTable, state: State, list: List[Form]): Node = list match {
    case op :: args => analyzeOp(table, state, op, args)
    case _ => Node.ListLit(List[Node]())
  }

  private def analyzeMap(table: SymbolTable, state: State, map: Map[Form, Form]): Node = {
    val analyzeFn = tryAnalyze(table, state.copy(atTopLevel = false)) _
    Node.MapLit(map.map({ case (k ,v) => (analyzeFn(k), analyzeFn(v)) }))
  }

  private def analyzeOp(table: SymbolTable, state: State, op: Form, args: List[Form]): Node = op match {
    case Form.SymF(s) => s match {
      case "def" => Def.analyze(table, state, args)
      case "let" => Let.analyze(table, state, args)
      case "if" => If.analyze(table, state, args)
      case "fn" => Fn.analyze(table, state, args)
      case "trait" => Trait.analyze(table, state, args)
      case "list" => ListExpr.analyze(table, state, args)
      case _ => Apply.analyze(table, state, s, args)
    }
    case u => {
      val message = s"""Type mismatch
                       |  ${op} is not a fn or special form""".stripMargin
      throw CompileError.TypeError(message)
    }
  }
}
