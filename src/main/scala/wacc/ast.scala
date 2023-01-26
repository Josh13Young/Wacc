package wacc

import parsley.Parsley
import parsley.implicits.zipped.Zipped2
import parsley.position.pos

object ast {

  // Generic Bridge Traits
  trait ParserBridgePos[+A] {
    def con(pos: (Int, Int)): A

    final def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con) <~ op
  }

  trait ParserBridgePos0[+A] extends ParserBridgePos[A] {
    def apply()(pos: (Int, Int)): A

    override def con(pos: (Int, Int)): A = apply()(pos)
  }

  trait ParserBridgePos1[-A, +B] extends ParserBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(apply(_) _)

    override def con(pos: (Int, Int)): A => B = apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C] extends ParserBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C

    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] = pos <**> (x, y).zipped(apply(_, _) _)

    override def con(pos: (Int, Int)): (A, B) => C = apply(_, _)(pos)
  }

  // node that holds position information (line, column)
  sealed trait ASTNode {
    val pos: (Int, Int)
  }

  // <PROGRAM>
  sealed trait Program extends ASTNode

  // Since Stat; Stat is valid, we represent it as a list of Stat
  case class BeginProgram(functions: List[Func], stat: List[Stat])(val pos: (Int, Int)) extends Program

  // <FUNC>

  case class Func(t: Type, ident: Ident, vars: List[Param], stat: List[Stat])(val pos: (Int, Int)) extends ASTNode

  // <PARAM-LIST> omitted (only 1 occurrence)

  // <PARAM>
  case class Param(t: Type, ident: Ident)(val pos: (Int, Int)) extends ASTNode

  // <STAT>
  sealed trait Stat extends ASTNode

  case class Skip()(val pos: (Int, Int)) extends Stat

  case class AssignNew(t: Type, ident: Ident, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat

  case class Assign(lvalue: Lvalue, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat

  case class Read(lvalue: Lvalue)(val pos: (Int, Int)) extends Stat

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat

  // Stat can call another Stat, so we represent it as a list of Stat
  case class If(cond: Expr, trueStat: List[Stat], falseStat: List[Stat])(val pos: (Int, Int)) extends Stat

  case class While(cond: Expr, stat: List[Stat])(val pos: (Int, Int)) extends Stat

  case class BeginStat(stat: List[Stat])(val pos: (Int, Int)) extends Stat

  // <LVALUE>
  sealed trait Lvalue extends ASTNode

  // <IDENT>
  case class Ident(name: String)(val pos: (Int, Int)) extends Lvalue with Expr

  object Ident {
    def apply(name: Parsley[String]): Parsley[Ident] =
      pos <**> name.map(Ident(_) _)
  }

  // <ARRAY-ELEM>
  case class ArrayElem(ident: Ident, exprList: List[Expr])(val pos: (Int, Int)) extends Lvalue with Expr

  object ArrayElem {
    def apply(ident: Parsley[Ident], exprList: Parsley[List[Expr]]): Parsley[ArrayElem] =
      pos <**> (ident, exprList).zipped(ArrayElem(_, _) _)
  }

  // <PAIR-ELEM>
  sealed trait PairElem extends Lvalue with Rvalue

  case class Fst(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem

  case class Snd(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem

  // <RVALUE>
  sealed trait Rvalue extends ASTNode

  // <ARRAY-LITER>
  case class ArrayLiter(exprList: List[Expr])(val pos: (Int, Int)) extends Rvalue

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue

  case class Call(ident: Ident, argList: List[Expr])(val pos: (Int, Int)) extends Rvalue // args-list only appears once

  // <TYPE>
  sealed trait Type extends ASTNode

  // <BASE-TYPE>
  sealed trait BaseType extends Type

  case class IntType()(val pos: (Int, Int)) extends BaseType

  case class BoolType()(val pos: (Int, Int)) extends BaseType

  case class CharType()(val pos: (Int, Int)) extends BaseType

  case class StringType()(val pos: (Int, Int)) extends BaseType

  // <ARRAY-TYPE>
  case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type

  // <PAIR-TYPE>
  case class PairType(t1: PairElemType, t2: PairElemType)(val pos: (Int, Int)) extends Type

  case class PairElemType(t: Type)(val pos: (Int, Int)) extends Type // Not too sure about this


  // <EXPR>
  sealed trait Expr extends Rvalue

  case class IntLiter(value: Int)(val pos: (Int, Int)) extends Expr

  object IntLiter {
    def apply(value: Parsley[Int]): Parsley[IntLiter] =
      pos <**> value.map(IntLiter(_) _)
  }

  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Expr

  object BoolLiter {
    def apply(value: Parsley[Boolean]): Parsley[BoolLiter] =
      pos <**> value.map(BoolLiter(_) _)
  }

  case class CharLiter(value: Char)(val pos: (Int, Int)) extends Expr

  object CharLiter {
    def apply(value: Parsley[Char]): Parsley[CharLiter] =
      pos <**> value.map(CharLiter(_) _)
  }

  case class StrLiter(value: String)(val pos: (Int, Int)) extends Expr

  object StrLiter {
    def apply(value: Parsley[String]): Parsley[StrLiter] =
      pos <**> value.map(StrLiter(_) _)
  }

  case class PairLiter()(val pos: (Int, Int)) extends Expr

  object PairLiter extends ParserBridgePos0[PairLiter]

  // <UNARY-OP>
  sealed trait UnaryOp extends Expr

  case class Not(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  object Not extends ParserBridgePos1[Expr, Not]

  case class Neg(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  object Neg extends ParserBridgePos1[Expr, Neg]


  case class Len(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  object Len extends ParserBridgePos1[Expr, Len]


  case class Ord(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  object Ord extends ParserBridgePos1[Expr, Ord]


  case class Chr(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  object Chr extends ParserBridgePos1[Expr, Chr]


  // <BINARY-OP>
  sealed trait BinaryOp extends Expr

  case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object Mul extends ParserBridgePos2[Expr, Expr, Mul]

  case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object Div extends ParserBridgePos2[Expr, Expr, Div]

  case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object Mod extends ParserBridgePos2[Expr, Expr, Mod]

  case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object Add extends ParserBridgePos2[Expr, Expr, Add]

  case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object Sub extends ParserBridgePos2[Expr, Expr, Sub]

  case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object GT extends ParserBridgePos2[Expr, Expr, GT]

  case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object GTE extends ParserBridgePos2[Expr, Expr, GTE]

  case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object LT extends ParserBridgePos2[Expr, Expr, LT]

  case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object LTE extends ParserBridgePos2[Expr, Expr, LTE]

  case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object EQ extends ParserBridgePos2[Expr, Expr, EQ]

  case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object NEQ extends ParserBridgePos2[Expr, Expr, NEQ]

  case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object And extends ParserBridgePos2[Expr, Expr, And]

  case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  object Or extends ParserBridgePos2[Expr, Expr, Or]
}