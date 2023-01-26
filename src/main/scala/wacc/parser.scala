package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, pure}
import parsley.combinator.endBy1
import parsley.expr.{InfixL, Ops, Prefix, precedence}
import wacc.ast._
import wacc.lexer._
import wacc.lexer.implicits.implicitSymbol

object parser {
  private lazy val ident = Ident(IDF)
  private lazy val boolLiter = BoolLiter("true" ~> pure(true) | "false" ~> pure(false))
  private lazy val intLiter = IntLiter(INT)
  private lazy val charLiter = CharLiter(CHR)
  private lazy val stringLiter = StrLiter(STR)
  private lazy val pairLiter = PairLiter <# "null" // what is this?
  private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))

  // expr ::= int | bool | char | string | pair | ident | arrayElem | "(" expr ")" | un-ops expr | expr bin-ops expr
  private lazy val atom: Parsley[Expr] = "(" *> expr <* ")" | attempt(arrayElem) | ident |
    stringLiter | intLiter | pairLiter | charLiter | boolLiter
  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(Prefix)(Not <# "!", Neg <# "-", Len <# "len", Ord <# "ord", Chr <# "chr"),
    Ops(InfixL)(Mul <# "*", Div <# "/", Mod <# "%"),
    Ops(InfixL)(Add <# "+", Sub <# "-"),
    Ops(InfixL)(EQ <# "=", NEQ <# "!=", GT <# ">", GTE <# ">=", LT <# "<", LTE <# "<="),
    Ops(InfixL)(And <# "&&"),
    Ops(InfixL)(Or <# "||"))
  val parser: Parsley[Expr] = fully(expr)
}
