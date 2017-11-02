package flop.analysis

import scala.collection.immutable.{List => SList}

sealed trait Type

object Type {
  case object Trait extends Type
  case object Self extends Type

  case object Unit extends Type
  case object Any extends Type

  case object Boolean extends Type

  case object Number extends Type
  case object String extends Type
  case object Symbol extends Type

  case class Generic(val name: String) extends Type

  abstract class Fn(val pTypes: SList[Type], val rType: Type) extends Type

  case class LuaFn(override val pTypes: SList[Type], override val rType: Type)
      extends Fn(pTypes, rType)

  case class FreeFn(override val pTypes: SList[Type], override val rType: Type)
      extends Fn(pTypes, rType)

  case class TraitFn(override val pTypes: SList[Type], override val rType: Type)
      extends Fn(pTypes, rType)

  case class Pack(val eType: Type) extends Type

  abstract class Aggregate(val types: SList[Type]) extends Type

  case class List(override val types: SList[Type]) extends Aggregate(types)
  case class Vector(override val types: SList[Type]) extends Aggregate(types)
  case class Map(override val types: SList[Type]) extends Aggregate(types)

}
