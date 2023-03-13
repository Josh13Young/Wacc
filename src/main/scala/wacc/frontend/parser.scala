package wacc.frontend

import parsley.Parsley
import parsley.Parsley.{attempt, notFollowedBy, pure}
import parsley.combinator._
import parsley.errors.combinator.ErrorMethods
import parsley.expr._
import wacc.ast._
import wacc.frontend.lexer._
import wacc.frontend.lexer.implicits.implicitSymbol

object parser {
  private lazy val program = Program("begin" ~> many(func), stat <~ "end")
  private lazy val func = attempt(Func(mainType, ident <~ "(", paramList <~ ") is", stat <~ "end").label("function definition"))

  // paramList can be empty
  private lazy val paramList = sepBy1(param, ",") | pure(List.empty)
  private lazy val param = Param(mainType, ident)

  // <stat>
  private lazy val statElem: Parsley[Stat] = Skip <# "skip" |
    AssignNew(mainType, ident, "=".label("new assignment") ~> rvalue) |
    Assign(lvalue, "=".label("assignment") ~> rvalue) |
    "read" ~> Read(lvalue) | "free" ~> Free(expr) | "return" ~> Return(expr) |
    "exit" ~> Exit(expr) | "print" ~> Print(expr) | "println" ~> Println(expr) |
    If("if".label("if statement") ~> expr, "then" ~> stat, "else" ~> stat <~ "fi") |
    While("while".label("while loop") ~> expr, "do" ~> stat <~ "done") |
    BeginStat("begin" ~> stat <~ "end")
  private lazy val stat: Parsley[List[Stat]] = sepBy1(statElem, ";".label("\";\"").hide)

  private lazy val lvalue: Parsley[Lvalue] = attempt(arrayElem) | ident | pairElem
  private lazy val pairElem = "fst" ~> FstElem(lvalue) | "snd" ~> SndElem(lvalue)

  private lazy val rvalue = expr | arrayLiter |
    "newpair".label("pair literal") ~> NewPair("(" ~> expr <~ ",", expr <~ ")") |
    "call".label("function call") ~> Call(ident, "(" ~> sepBy(expr, ",") <~ ")") | pairElem

  // <type>
  private lazy val mainType: Parsley[Type] = attempt(baseType <~ notFollowedBy("[")) |
    attempt(pairType <~ notFollowedBy("[")) | arrayType
  private lazy val baseType = IntType <# "int" | BoolType <# "bool" | CharType <# "char" |
    StringType <# "string"
  private lazy val arrayType = precedence[Type](baseType | pairType)(
    Ops(Postfix)(ArrayType <# "[" ~> "]"))
  private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val pairElemType: Parsley[Type] = pairType |
    attempt(baseType <~ notFollowedBy("[")) | arrayType

  // <expr>
  private lazy val atom: Parsley[Expr] = intLiter | "(".label("open parenthesis") ~> expr <~ ")".label("close parenthesis") | attempt(arrayElem) | ident |
    stringLiter | pairLiter | charLiter | boolLiter | negate
  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(Prefix)(Not <# "!".label("unary operator"), Len <# "len".label("unary operator"), Ord <# "ord".label("unary operator"), Chr <# "chr".label("unary operator")),
    Ops(InfixL)(Mul <# "*".label("binary operator"), Div <# "/".label("binary operator"), Mod <# "%".label("binary operator")),
    Ops(InfixL)(Add <# "+".label("binary operator"), Sub <# "-".label("binary operator")),
    Ops(InfixL)(EQ <# "==".label("binary operator"), NEQ <# "!=".label("binary operator"), GT <# ">".label("binary operator"), GTE <# ">=".label("binary operator"), LT <# "<".label("binary operator"), LTE <# "<=".label("binary operator")),
    Ops(InfixL)(And <# "&&".label("binary operator")),
    Ops(InfixL)(Or <# "||".label("binary operator")),
  )

  private lazy val ident = Ident(IDF)
  private lazy val negate = Neg("-" ~> expr).hide
  private lazy val arrayElem = ArrayElem(ident, endBy1("[".label("index").explain("index like list[index]") ~> expr, "]"))
  private lazy val intLiter = IntLiter(INT)
  private lazy val boolLiter = BoolLiter("true" ~> pure(true) | "false" ~> pure(false)).label("boolean literal")
  private lazy val charLiter = CharLiter(CHR)
  private lazy val stringLiter = StrLiter(STR)
  private lazy val arrayLiterElems = sepBy1(expr, ",") | pure(List.empty)
  private lazy val arrayLiter = ArrayLiter("[" ~> arrayLiterElems <~ "]").label("array literal")
  private lazy val pairLiter = PairLiter <# "null"

  val parser: Parsley[Program] = fully(program)
  val funcParser: Parsley[List[Func]] = fully(many(func))
}
