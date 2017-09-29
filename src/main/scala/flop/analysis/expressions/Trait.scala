package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Name._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._

object Trait {

  def analyze(table: SymbolTable, state: State, args: List[Form]): Node = {
    if (!state.atTopLevel) {
      val message =
        """Unexpected trait form in a nested scope
          |  trait definitions must occur at a module top level""".stripMargin
      throw syntaxError(message)
    } else if (args.length != 2) {
      val message = s"""Wrong number of terms in trait form"
                       |  expected: 2, actual: ${args.length}""".stripMargin
      throw syntaxError(message)
    } else if (!args.head.isInstanceOf[Form.SymF]) {
      val message = s"""trait form expects NAME to be a symbol
                       |  got: ${args.head}""".stripMargin
      throw syntaxError(message)
    } else {
      // TODO check if symbol contains('.')
      val symbolText = args.head.asInstanceOf[Form.SymF].value

      val traitName = Name.ModuleName.nest(state.currentModule, symbolText)
      if (SymbolTable.lookupTrait(table, traitName).nonEmpty) {
        val message = s"trait ${symbolText} already defined in this module"
        throw CompileError.moduleError(message)
      }
      val name = Name.ModuleName.nest(state.currentModule, symbolText)
      val symbol = Node.SymLit(name, Type.Trait)
      val fnDefs = analyzeFnDefs(table, state, symbol, args(1))

      Node.TraitN(symbol, fnDefs)
    }
  }

  private def syntaxError(specificMessage: String) = {
    val genericMessage =
      """trait definitions must be of the form:
        |  (trait NAME FN-DEFS)
        |  where:
        |    NAME is a (capitalized) unqualified symbol
        |    FN-DEFS is a map of the form:
        |      {name1 fn-def1 name2 fn-def2 namen fn-defn}
        |      where:
        |        fn-def is of the form:
        |          (RETURN-TYPE {PARAMS})""".stripMargin

    CompileError.syntaxError(specificMessage, genericMessage)
  }

  private def analyzeFnDefs(table: SymbolTable, state: State, traitName: Node.SymLit, form: Form): Map[Node.SymLit, Node.FnDef] = {
    val rawDefs = form match {
      case Form.MapF(raw) => raw
      case _ => {
        val message = s"""trait form expects FN-DEFS to be a map
                         |  got: ${form}""".stripMargin
        throw syntaxError(message)
      }
    }

    rawDefs.map({ case (n, f) => analyzeFnDef(table, state, traitName, n, f) }).toMap
  }

  private def analyzeFnDef(table: SymbolTable, state: State, traitName: Node.SymLit, fName: Form, fnDef: Form): (Node.SymLit, Node.FnDef) = {
    val rawName = fName match {
      case Form.SymF(value) => value
      case _ => {
        val message = s"""trait form expects FN-DEF NAME to be a symbol
                         |  got: ${fName}""".stripMargin
        throw syntaxError(message)
      }
    }
    val rawFnDef = fnDef match {
      case Form.ListF(values) => values
      case _ => {
        val message = s"""trait form expects FN-DEF to be of the proper form
                         |  got: ${fnDef}""".stripMargin
        throw syntaxError(message)
      }
    }
    if (rawFnDef.length < 1) {
      val message = s"""Wrong number of terms in trait FN-DEF"
                       |  expected (1..N), actual: ${rawFnDef.length}""".stripMargin
      throw syntaxError(message)
    }

    val types = rawFnDef.map(t => TypeExpr.analyze(table, t))
    val fnType = Type.TraitFn(types.tail, types.head)
    val name = Name.ModuleName.nest(state.currentModule, rawName)
    val fnName = Node.SymLit(name, fnType)

    (fnName, Node.FnDef(traitName, fnName, fnType))
  }
}
