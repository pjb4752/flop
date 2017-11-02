package flop.analysis.expressions

import scala.collection.immutable.{List => SList, Map => SMap}

import flop.analysis._
import flop.analysis.Type._
import flop.analysis.Node._
import flop.reading.Form
import flop.reading.Form._
import flop.stdlib.Core

object Apply {

  def analyze(table: SymbolTable, state: State, op: String, args: SList[Form]): Node = {
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

  type GTypes = SMap[String, Type]
  private def getWrappedType(t: Type, gTypes: GTypes): Type = t match {
    case a: Type.Aggregate => {
      val mapped = a.types.map(w => getWrappedType(w, gTypes))
      a match {
        case _: Type.List => Type.List(mapped)
        case _: Type.Vector => Type.Vector(mapped)
        case _: Type.Map => Type.Map(mapped)
      }
    }
    case Type.Generic(name) => gTypes(name)
    case _ => t
  }

  private def analyzeFreeFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.FreeFn, args: SList[Form]): Node = {
    val (arguments, gTypes) = analyzeArgs(table, state, op, fnType, args)

    val realRType = getWrappedType(fnType.rType, gTypes)
    Node.FlopApply(op, arguments, realRType)
  }

  private def analyzeLuaFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.LuaFn, args: SList[Form]): Node = {
    val (arguments, gTypes) = analyzeArgs(table, state, op, fnType, args)
    // TODO it should alread exist here since checked above
    val moduleVar = SymbolTable.lookupVar(table, op).get
    val luaFn = moduleVar.node.asInstanceOf[Node.LuaFn]

    val realRType = getWrappedType(fnType.rType, gTypes)
    Node.LuaApply(luaFn, arguments, realRType)
  }

  // TODO test this
  private def analyzeTraitFn(table: SymbolTable, state: State, op: Name,
      fnType: Type.TraitFn, args: SList[Form]): Node = {

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
      fnType: Type.Fn, args: SList[Form]): (SList[Node], GTypes) = {
    val nestedState = state.copy(atTopLevel = false)

    def getNodeType(node: Node): Type = node match {
      case Node.SymLit(v, _) => SymbolTable.lookupType(table, state, v)
      case _ => node.eType
    }

    def analyzeGenType(name: String, eType: Type, gTypes: GTypes): (Type, GTypes) = {
      if (gTypes.contains(name)) {
        val existingType = gTypes(name)
        if (existingType != eType) {
          val target = s"${op.toString}, generic type: ${name}"
          throw CompileError.typeError(target, existingType, eType)
        } else {
          (existingType, gTypes)
        }
      } else {
        val newGTypes = gTypes + (name -> eType)
        (eType, newGTypes)
      }
    }

    def capturePack(packType: Type, pack: SList[(Form, Int)], gTypes: GTypes): (SList[Node], GTypes) = {

      pack.foldLeft((SList[Node](), gTypes))({ case ((ns, gt), (form, i)) => {
          val node = state.analyzeFn(table, nestedState)(form)
          val mType = getNodeType(node)

          val (wrappedType, newGt) = packType match {
            case Type.Generic(name) => analyzeGenType(name, mType, gt)
            case _ => (packType, gt)
          }

          if (mType != wrappedType) {
            val target = s"${op.toString}, param: (${i})"
            throw CompileError.typeError(target, wrappedType, mType)
          } else {
            (ns :+ node, newGt)
          }
        }
      })
    }

    def analyzeArg(pType: Type, arg: Node, aType: Type, index: Int,
        gTypes: GTypes): GTypes = {

      pType match {
        case Type.Generic(name) => {
          val (_, newGt) = analyzeGenType(name, aType, gTypes)
          newGt
        }
        case _ if pType == aType => gTypes
        case _ => {
          val target = s"${op.toString}, param: ${index}"
          throw CompileError.typeError(target, pType, aType)
        }
      }
    }

    def analyzeAgg(agg: Type.Aggregate, arg: Node, aType: Type, index: Int,
        gTypes: GTypes): GTypes = {

      if (agg.getClass != aType.getClass) {
        val target = s"${op.toString}, param: ${index}"
        throw CompileError.typeError(target, agg, aType)
      }
      val argType = aType.asInstanceOf[Type.Aggregate]

      agg.types.zipWithIndex.foldLeft(gTypes)({ case (gt, (t, i)) =>
        val curArgType = argType.types(i)

        val (realType, newGt) = t match {
          case Type.Generic(name) => analyzeGenType(name, curArgType, gt)
          case _ => (t, gt)
        }

        if (realType != curArgType) {
          val target = s"${op.toString}, param: ${index}"
          throw CompileError.typeError(target, realType, curArgType)
        }
        newGt
      })
    }

    def analyzeArgs0(arguments: SList[(Form, Int)], gTypes: GTypes):
        (SList[Node], GTypes) = {
      if (arguments.isEmpty) {
        (SList[Node](), gTypes)
      } else {
        val (form, i) :: tail = arguments
        val pType = fnType.pTypes(i)

        pType match {
          case Type.Pack(wrapped) => capturePack(wrapped, arguments, gTypes)
          case _ => {
            val arg = state.analyzeFn(table, nestedState)(form)
            val aType = getNodeType(arg)

            val newGTypes = pType match {
              case a: Type.Aggregate => analyzeAgg(a, arg, aType, i, gTypes)
              case _ => analyzeArg(pType, arg, aType, i, gTypes)
            }

            val (nextNodes, allGTypes) = analyzeArgs0(tail, newGTypes)
            (arg :: nextNodes, allGTypes)
          }
        }
      }
    }

    checkArity(op.name, fnType, args)
    analyzeArgs0(args.zipWithIndex, SMap[String, Type]())
  }

  private def typecheckTraitArgs(table: SymbolTable, state: State, op: Name,
      fnType: Type.Fn, args: SList[Node], selfType: Type): Unit = {

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

  private def checkArity(op: String, fnType: Type.Fn, args: SList[Form]): Unit = {
    val isVarArg = fnType.pTypes.exists(p => p.isInstanceOf[Type.Pack])

    if (!isVarArg) {
      val arity = fnType.pTypes.length
      if (args.length != arity) {
        val expected = (arity, arity)
        throw CompileError.argumentError(op.toString, expected, args.length)
      }
    }
  }

  private def analyzeArgForms(table: SymbolTable, state: State,
      args: SList[Form]): SList[Node] = {

    args.map(state.analyzeFn(table, state.copy(atTopLevel = false)))
  }
}
