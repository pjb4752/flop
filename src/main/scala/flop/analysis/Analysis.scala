package flop.analysis

import scala.collection.immutable.{Map => SMap}

import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core
import flop.stdlib.core.{Map, Pair}

import flop.analysis.expressions._

object Analysis {

  def analyzeModuleDef(table: SymbolTable, header: Form): ModuleTree.Module = {
    Module.analyze(table, header)
  }

  def analyze(table: SymbolTable, module: ModuleTree.Module,
      forms: List[Form]): (SymbolTable, List[Node]) = {
    val state = State(tryAnalyze, true, module.name)

    val initial = (table, List[Node]())
    val (finalTable, ast) = forms.foldLeft(initial)({ case ((tbl, ast), form) => {
        val newNode = tryAnalyze(tbl, state)(form)
        val newTable = newNode match {
          case Node.DefN(n, e, t) => {
            val vName = n.name.asInstanceOf[Name.ModuleName]
            val varName = Name.ModuleName.nest(module.name, vName.name)
            SymbolTable.addVar(tbl, varName, e)
          }
          case Node.TraitN(n, fns) => {
            val tName = n.name.asInstanceOf[Name.ModuleName]
            val traitName = Name.ModuleName.nest(module.name, tName.name)
            val fnDefs = fns.map({ case (s, fn) =>
              val fName = s.name.name
              (fName, ModuleTree.Module.FnDef(fName, fn))
            })

            SymbolTable.addTrait(tbl, traitName, fnDefs)
          }
          case _ => tbl
        }

        (newTable, newNode :: ast)
      }
    })

    (finalTable, ast.reverse)
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
    case _ => throw CompileError.syntaxError("", "empty expression ()")
  }

  private def analyzeOp(table: SymbolTable, state: State, op: Form, args: List[Form]): Node = op match {
    case Form.SymF(s) => s match {
      case "def" => Def.analyze(table, state, args)
      case "let" => Let.analyze(table, state, args)
      case "if" => If.analyze(table, state, args)
      case "fn" => Fn.analyze(table, state, args)
      case "trait" => Trait.analyze(table, state, args)
      case _ => Apply.analyze(table, state, s, args)
    }
    case u => {
      val message = s"""Type mismatch
                       |  ${op} is not a fn or special form""".stripMargin
      throw CompileError.TypeError(message)
    }
  }

  private def analyzeMap(table: SymbolTable, state: State, map: SMap[Form, Form]): Node = {
    val analyzeFn = tryAnalyze(table, state.copy(atTopLevel = false)) _
    val pairFnName = Form.SymF(Pair.newName.toFlop)
    val mapFnName = Map.newName.toFlop

    val args = map.map({ case (k, v) =>
      Form.ListF(List(pairFnName, k, v)) }).toList

    Apply.analyze(table, state, mapFnName, args)
  }
}
