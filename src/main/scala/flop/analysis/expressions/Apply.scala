package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Type._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Apply {

  def analyze(table: SymbolTable, state: State, op: String, args: List[Form]): Node = {
    val maybeName = SymbolTable.lookupName(table, state, op)

    if (maybeName.isEmpty) {
      throw CompileError.undefinedError(op)
    }

    val name = maybeName.get
    SymbolTable.lookupType(table, state, name) match {
      case ff: Type.FreeFn => analyzeApplyFreeFn(table, state, name, ff, args)
      case lf: Type.LuaFn => analyzeApplyLuaFn(table, state, name, lf, args)
      case tf: Type.TraitFn => analyzeApplyTraitFn(table, state, name, tf, args)
      case t => throw CompileError.typeError(op, "Function", t)
    }
  }

  private def analyzeApplyFreeFn(table: SymbolTable, state: State, op: Name, fnType: Type.FreeFn,
       args: List[Form]): Node = {
    val arguments = analyzeArguments(table, state, op, fnType, args)

    Node.FlopApply(op, arguments, fnType.rType)
  }

  private def analyzeApplyLuaFn(table: SymbolTable, state: State, op: Name, fnType: Type.LuaFn,
      args: List[Form]): Node = {
    val arguments = analyzeArguments(table, state, op, fnType, args)
    // TODO it should alread exist here since checked above
    val moduleVar = SymbolTable.lookupVar(table, op).get
    val luaFn = moduleVar.node.asInstanceOf[Node.LuaFn]

    Node.LuaApply(luaFn, arguments, fnType.rType)
  }

  private def analyzeApplyTraitFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.TraitFn, args: List[Form]): Node = {

    // TODO For now we assume that any trait takes "self" as first param
    // but in the future we'll need to type annotate somehow for traitFns
    // that might not take a self parameter, or come up with another solution
    val arguments = analyzeArguments(table, state, op, fnType, args, Some(0))
    val selfType = arguments.head
    val traitFnImpl = SymbolTable.lookupTraitFnImpl(table, state, op, selfType.eType)

    if (traitFnImpl.isEmpty) {
      throw CompileError.unimplementedError(op.name, selfType.eType)
    }

    // at this point none of this should blow up
    val rType = traitFnImpl.get.rType

    traitFnImpl.get match {
      case lf: Node.LuaFn => Node.LuaApply(lf, arguments, rType)
      case ff: Node.FlopFn => Node.FlopApply(op, arguments, rType)
    }
  }

  private def analyzeArguments(table: SymbolTable, state: State, op: Name, fnType: Type.Fn,
      args: List[Form], selfPos: Option[Int] = None): List[Node] = {
    val arity = fnType.pTypes.length
    if (args.length != arity) {
      val expected = (arity, arity)
      throw CompileError.argumentError(op.toString, expected, args.length)
    }

    val arguments = args.map(state.analyzeFn(table, state.copy(atTopLevel = false)))
    for ((arg, i) <- arguments.zipWithIndex) {
      val aType = arg match {
        case Node.SymLit(v, _) => SymbolTable.lookupType(table, state, v)
        case _ => arg.eType
      }
      val pType = fnType.pTypes(i)
      if (selfPos.nonEmpty && selfPos.get == i) {
        // don't check type of selfType
      } else if (pType != aType) {
        val target = s"${op.toString}, param: ${i}"
        throw CompileError.typeError(target, pType, aType)
      }
    }

    arguments
  }
}
