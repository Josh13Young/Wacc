package wacc

import wacc.ast._

object STType {
  sealed trait TypeST

  case class IntST() extends TypeST

  case class BoolST() extends TypeST

  case class CharST() extends TypeST

  case class StringST() extends TypeST

  case class ArrayST(t: TypeST) extends TypeST

  case class PairST(t1: TypeST, t2: TypeST) extends TypeST

  case class AnyST() extends TypeST

  case class VoidST() extends TypeST

  def typeConvert(t: Type): TypeST = t match {
    case IntType() => IntST()
    case BoolType() => BoolST()
    case CharType() => CharST()
    case StringType() => StringST()
    case ArrayType(t) => ArrayST(typeConvert(t))
    case PairType(t1, t2) => PairST(typeConvert(t1), typeConvert(t2))
    case NestedPairType() => PairST(AnyST(), AnyST())
    case _ => throw new Exception("Invalid type") // unreachable
  }

  def typeCompare(t1: TypeST, t2: TypeST): TypeST =
    (t1, t2) match {
    case (IntST(), IntST()) => IntST()
    case (BoolST(), BoolST()) => BoolST()
    case (CharST(), CharST()) => CharST()
    case (StringST(), StringST()) => StringST()
    case (ArrayST(t1), ArrayST(t2)) => typeCompare(t1, t2)
    case (PairST(t11, t12), PairST(t21, t22)) => PairST(typeCompare(t11, t21), typeCompare(t12, t22))
    case (AnyST(), x) => x
    case (x, AnyST()) => x
    case _ => VoidST()
  }

}
