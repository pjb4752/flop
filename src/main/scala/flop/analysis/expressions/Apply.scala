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
    } else {
      val name = maybeName.get
      SymbolTable.lookupType(table, state, name) match {
        case ff: Type.FreeFn => analyzeFreeFn(table, state, name, ff, args)
        case lf: Type.LuaFn => analyzeLuaFn(table, state, name, lf, args)
        case tf: Type.TraitFn => analyzeTraitFn(table, state, name, tf, args)
        case t => throw CompileError.typeError(op, "Function", t)
      }
    }
  }

  private def analyzeFreeFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.FreeFn, args: List[Form]): Node = {
    val arguments = analyzeArgs(table, state, op, fnType, args)

    Node.FlopApply(op, arguments, fnType.rType)
  }

  private def analyzeLuaFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.LuaFn, args: List[Form]): Node = {
    val arguments = analyzeArgs(table, state, op, fnType, args)
    // TODO it should alread exist here since checked above
    val moduleVar = SymbolTable.lookupVar(table, op).get
    val luaFn = moduleVar.node.asInstanceOf[Node.LuaFn]

    Node.LuaApply(luaFn, arguments, fnType.rType)
  }

  // TODO test this
  private def analyzeTraitFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.TraitFn, args: List[Form]): Node = {

    checkArity(op.name, fnType, args)
    val arguments = analyzeArgForms(table, state, args)

    val selfPos = fnType.pTypes.zipWithIndex.filter({ case (t, i) =>
      t match {
        case _: Type.Self.type => true
        case _ => false
      }
    }).map(_._2)

    val selfTypes = selfPos.map(i => arguments(i).eType).groupBy(t => t).
        map(_._1).toList

    if (selfTypes.size > 1) {
      throw CompileError.selfTypeError(op.name, selfTypes)
    }

    val selfType = selfTypes.head
    val traitFnImpl = SymbolTable.lookupTraitFnImpl(table, state, op, selfType)

    if (traitFnImpl.isEmpty) {
      throw CompileError.unimplementedError(op.name, selfType)
    }

    typecheckTraitArgs(table, state, op, fnType, arguments, selfType)

    // at this point none of this should blow up
    val rType = if (traitFnImpl.get.rType == Type.Self) {
      selfType
    } else {
      traitFnImpl.get.rType
    }

    traitFnImpl.get match {
      case lf: Node.LuaFn => Node.LuaApply(lf, arguments, rType)
      case ff: Node.FlopFn => Node.FlopApply(op, arguments, rType)
    }
  }

  private def analyzeArgs(table: SymbolTable, state: State, op: Name,
      fnType: Type.Fn, args: List[Form]): List[Node] = {

    checkArity(op.name, fnType, args)
    val arguments = analyzeArgForms(table, state, args)

    for ((arg, i) <- arguments.zipWithIndex) {
      val aType = arg match {
        case Node.SymLit(v, _) => SymbolTable.lookupType(table, state, v)
        case _ => arg.eType
      }
      val pType = fnType.pTypes(i)
      if (pType != aType) {
        val target = s"${op.toString}, param: ${i}"
        throw CompileError.typeError(target, pType, aType)
      }
    }

    arguments
  }

  private def typecheckTraitArgs(table: SymbolTable, state: State, op: Name,
      fnType: Type.Fn, args: List[Node], selfType: Type): Unit = {

    for ((arg, i) <- args.zipWithIndex) {
      val aType = arg match {
        case Node.SymLit(v, _) => SymbolTable.lookupType(table, state, v)
        case _ => arg.eType
      }
      val pType = fnType.pTypes(i)
      if (pType != Type.Self && pType != aType) {
        val target = s"${op.toString}, param: ${i}"
        throw CompileError.typeError(target, pType, aType)
      }
    }
  }

  private def checkArity(op: String, fnType: Type.Fn, args: List[Form]): Unit = {
    val arity = fnType.pTypes.length
    if (args.length != arity) {
      val expected = (arity, arity)
      throw CompileError.argumentError(op.toString, expected, args.length)
    }
  }

  private def analyzeArgForms(table: SymbolTable, state: State,
      args: List[Form]): List[Node] = {

    args.map(state.analyzeFn(table, state.copy(atTopLevel = false)))
  }
}
