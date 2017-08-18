package flop.analysis.expressions

import flop.analysis._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Apply {

  type FlopError = flop.analysis.Error

  case class ApplyError(op: String, opType: Type) extends FlopError {
    val message = s"attempt to apply ${op} which is type ${opType}"
  }

  case class UnimplementedError(op: String, selfType: Type) extends FlopError {
    val message = s"""Error applying trait function:
                     |Unimplemented FN ${op} for ${selfType}""".stripMargin
  }

  case class ArityMismatchError(op: String, formal: Int, actual: Int)
      extends FlopError {

    val message = s"arity mismatch in ${op}, expected ${formal}, got ${actual}"
  }

  case class ParamTypeError(i: Int, formal: Type, actual: Type)
      extends FlopError {

    val message = s"param ${i} expected type ${formal}, got ${actual}"
  }

  def analyze(tree: ModuleTree, state: State, op: String, args: List[Form]): Node = {
    SymbolTable.lookupType(tree, state, op) match {
      case ff: Type.FreeFn => analyzeApplyFreeFn(tree, state, op, ff, args)
      case lf: Type.LuaFn => analyzeApplyLuaFn(tree, state, op, lf, args)
      case tf: Type.TraitFn => analyzeApplyTraitFn(tree, state, op, tf, args)
      case t => throw CompileError(ApplyError(op, t))
    }
  }

  private def analyzeApplyFreeFn(tree: ModuleTree, state: State, op: String, fnType: Type.FreeFn,
       args: List[Form]): Node = {
    val arguments = analyzeArguments(tree, state, op, fnType, args)

    Node.FlopApply(op, arguments, fnType.rType)
  }

  private def analyzeApplyLuaFn(tree: ModuleTree, state: State, op: String, fnType: Type.LuaFn,
      args: List[Form]): Node = {
    val arguments = analyzeArguments(tree, state, op, fnType, args)
    val luaFn = Core.builtins(op).asInstanceOf[Node.LuaFn]

    Node.LuaApply(luaFn, arguments, fnType.rType)
  }

  private def analyzeApplyTraitFn(tree: ModuleTree, state: State, op: String,
      fnType: Type.TraitFn, args: List[Form]): Node = {

    // TODO For now we assume that any trait takes "self" as first param
    // but in the future we'll need to type annotate somehow for traitFns
    // that might not take a self parameter, or come up with another solution
    val arguments = analyzeArguments(tree, state, op, fnType, args, Some(0))
    val selfType = arguments.head
    val traitImpl = SymbolTable.findTraitImpl(tree, state, op, selfType.eType)

    if (traitImpl.isEmpty) {
      throw CompileError(UnimplementedError(op, selfType.eType))
    }

    // at this point none of this should blow up
    val fnImpl = traitImpl.get.fnImpls.find(_._1.value == op).get._2
    val rType = fnImpl.rType

    fnImpl match {
      case lf: Node.LuaFn => Node.LuaApply(lf, arguments, rType)
      case ff: Node.FlopFn => Node.FlopApply(op, arguments, rType)
    }
  }

  private def analyzeArguments(tree: ModuleTree, state: State, op: String, fnType: Type.Fn,
      args: List[Form], selfPos: Option[Int] = None): List[Node] = {
    val arity = fnType.pTypes.length
    if (args.length != arity) {
      throw CompileError(ArityMismatchError(op, arity, args.length))
    }

    val arguments = args.map(state.analyzeFn(tree, state.copy(atTopLevel = false)))
    for ((arg, i) <- arguments.zipWithIndex) {
      val aType = arg match {
        case Node.SymLit(v, _) => SymbolTable.lookupType(tree, state, v)
        case _ => arg.eType
      }
      val pType = fnType.pTypes(i)
      if (selfPos.nonEmpty && selfPos.get == i) {
        // don't check type of selfType
      } else if (pType != aType) {
        throw CompileError(ParamTypeError(i, pType, aType))
      }
    }

    arguments
  }
}
