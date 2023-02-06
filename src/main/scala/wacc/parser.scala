package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.combinator._
import parsley.expr._
import wacc.ast._
import wacc.lexer._
import wacc.lexer.implicits.implicitSymbol

object parser {
  private lazy val program = Program("begin" ~> many(func), stat <~ "end")
  private lazy val func = attempt(Func(mainType, ident <~ "(", paramList <~ ") is", stat <~ "end"))

  // paramList can be empty
  private lazy val paramList = sepBy1(param, ",") | pure(List.empty)
  private lazy val param = Param(mainType, ident)

  // <stat>
  private lazy val statElem: Parsley[Stat] = Skip <# "skip" |
    AssignNew(mainType, ident, "=" ~> rvalue) |
    Assign(lvalue, "=" ~> rvalue) |
    "read" ~> Read(lvalue) | "free" ~> Free(expr) | "return" ~> Return(expr) |
    "exit" ~> Exit(expr) | "print" ~> Print(expr) | "println" ~> Println(expr) |
    If("if" ~> expr, "then" ~> stat, "else" ~> stat <~ "fi") |
    While("while" ~> expr, "do" ~> stat <~ "done") |
    BeginStat("begin" ~> stat <~ "end")
  private lazy val stat: Parsley[List[Stat]] = sepBy1(statElem, ";")

  private lazy val lvalue: Parsley[Lvalue] = attempt(arrayElem) | ident | pairElem
  private lazy val pairElem = "fst" ~> FstElem(lvalue) | "snd" ~> SndElem(lvalue)

  private lazy val rvalue = expr | arrayLiter |
    "newpair" ~> NewPair("(" ~> expr <~ ",", expr <~ ")") |
    "call" ~> Call(ident, "(" ~> sepBy(expr, ",") <~ ")") | pairElem

  // <type>
  private lazy val mainType: Parsley[Type] = attempt(baseType <~ notFollowedBy("[")) |
    attempt(pairType <~ notFollowedBy("[")) | arrayType
  private lazy val baseType = IntType <# "int" | BoolType <# "bool" | CharType <# "char" |
    StringType <# "string"
  private lazy val arrayType = precedence[Type](baseType | pairType)(
    Ops(Postfix)(ArrayType <# "[" ~> "]"))
  private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val pairElemType: Parsley[Type] = NestedPairType <# "pair" |
    attempt(baseType <~ notFollowedBy("[")) | arrayType

  // <expr>
  private lazy val atom: Parsley[Expr] = intLiter | "(" ~> expr <~ ")" | attempt(arrayElem) | ident |
    stringLiter | pairLiter | charLiter | boolLiter | negate
  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(Prefix)(Not <# "!", Len <# "len", Ord <# "ord", Chr <# "chr"),
    Ops(InfixL)(Mul <# "*", Div <# "/", Mod <# "%"),
    Ops(InfixL)(Add <# "+", Sub <# "-"),
    Ops(InfixL)(EQ <# "==", NEQ <# "!=", GT <# ">", GTE <# ">=", LT <# "<", LTE <# "<="),
    Ops(InfixL)(And <# "&&"),
    Ops(InfixL)(Or <# "||"),
  )

  private lazy val ident = Ident(IDF)
  private lazy val negate = Neg("-" ~> expr)
  private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))
  private lazy val intLiter = IntLiter(INT)
  private lazy val boolLiter = BoolLiter("true" ~> pure(true) | "false" ~> pure(false))
  private lazy val charLiter = CharLiter(CHR)
  private lazy val stringLiter = StrLiter(STR)
  private lazy val arrayLiterElems = sepBy1(expr, ",") | pure(List.empty)
  private lazy val arrayLiter = ArrayLiter("[" ~> arrayLiterElems <~ "]")
  private lazy val pairLiter = PairLiter <# "null" // what is this?

  val parser: Parsley[Program] = fully(program)
}
