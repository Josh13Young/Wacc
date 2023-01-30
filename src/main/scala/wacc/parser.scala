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
  private lazy val func = attempt(Func(_type, ident <~ "(", paramList <~ ") is", stat <~ "end"))
  // paramList can be empty
  private lazy val paramList = sepBy1(param, ",") | pure(List.empty)
  private lazy val param = Param(_type, ident)

  // stat ::= skip | free expr | return expr | exit expr | print expr | println expr | ifStat | whileStat
  //         | begin stat end
  private lazy val statElem: Parsley[Stat] = Skip <# "skip" | AssignNew(_type, ident, "=" ~> rvalue) |
    Assign(lvalue, "=" ~> rvalue) | "read" ~> Read(lvalue) | "free" ~> Free(expr) |
    "return" ~> Return(expr) | "exit" ~> Exit(expr) | "print" ~> Print(expr) |
    "println" ~> Println(expr) | ifStat | whileStat | BeginStat("begin" ~> stat <~ "end")
  // has to be sepBy1 since the last semicolon must be omitted (bad)
  private lazy val stat: Parsley[List[Stat]] = sepBy1(statElem, ";")
  // ifStat ::= if expr then stat else statList fi
  private lazy val ifStat = If("if" ~> expr, "then" ~> stat, "else" ~> stat <~ "fi")
  // whileStat ::= while expr do statList done
  private lazy val whileStat = While("while" ~> expr, "do" ~> stat <~ "done")

  private lazy val lvalue: Parsley[Lvalue] = attempt(arrayElem) | ident | pairElem
  private lazy val pairElem = "fst" ~> FstElem(lvalue) | "snd" ~> SndElem(lvalue)

  private lazy val rvalue = expr | arrayLiter | "newpair" ~> NewPair("(" ~> expr <~ ",", expr <~ ")") |
    "call" ~> Call(ident, "(" ~> sepBy(expr, ",") <~ ")") | pairElem

  private lazy val _type: Parsley[Type] = attempt(baseType <~ notFollowedBy("[")) |
    attempt(pairType <~ notFollowedBy("[")) | arrayType
  private lazy val baseType = IntType <# "int" | BoolType <# "bool" | CharType <# "char" |
    StringType <# "string"
  //  private lazy val arrayType = ArrayType(baseType <~ ("[" ~> "]")) | ArrayType(pairType <~ ("[" ~> "]"))
  private lazy val arrayType = precedence[Type](baseType | pairType)(
    Ops(Postfix)(ArrayType <# "[" ~> "]"))
  private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val pairElemType: Parsley[Type] = NestedPairType <# "pair" | attempt(baseType <~ notFollowedBy("[")) | arrayType

  // expr ::= int | bool | char | string | pair | ident | arrayElem | "(" expr ")" | un-ops expr | expr bin-ops expr
  private lazy val atom: Parsley[Expr] = "(" ~> expr <~ ")" | attempt(arrayElem) | ident |
    stringLiter | intLiter | pairLiter | charLiter | boolLiter
  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(Prefix)(Not <# "!", Neg <# "-", Len <# "len", Ord <# "ord", Chr <# "chr"),
    Ops(InfixL)(Mul <# "*", Div <# "/", Mod <# "%"),
    Ops(InfixL)(Add <# "+", Sub <# "-"),
    Ops(InfixL)(EQ <# "==", NEQ <# "!=", GT <# ">", GTE <# ">=", LT <# "<", LTE <# "<="),
    Ops(InfixL)(And <# "&&"),
    Ops(InfixL)(Or <# "||"))
  private lazy val ident = Ident(IDF)
  private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))
  private lazy val intLiter = optional("+") ~> IntLiter(INT)
  private lazy val boolLiter = BoolLiter("true" ~> pure(true) | "false" ~> pure(false))
  private lazy val charLiter = CharLiter(CHR)
  private lazy val stringLiter = StrLiter(STR)
  private lazy val arrayLiterElems = sepBy1(expr, ",") | pure(List.empty)
  private lazy val arrayLiter = ArrayLiter("[" ~> arrayLiterElems <~ "]")
  private lazy val pairLiter = PairLiter <# "null" // what is this?

  val parser: Parsley[Program] = fully(program)
}
