package wacc

object ast {
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

  // <ARRAY-ELEM>
  case class ArrayElem(ident: Ident, exprList: List[Expr])(val pos: (Int, Int)) extends Lvalue with Expr

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

  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Expr

  case class CharLiter(value: Character)(val pos: (Int, Int)) extends Expr

  case class StrLiter(value: String)(val pos: (Int, Int)) extends Expr

  case class PairLiter()(val pos: (Int, Int)) extends Expr

  // <UNARY-OP>
  sealed trait UnaryOp extends Expr

  case class Not(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  case class Neg(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  case class Len(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  case class Ord(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  case class Chr(expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  // <BINARY-OP>
  sealed trait BinaryOp extends Expr

  case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp

  case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp
}