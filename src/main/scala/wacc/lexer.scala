package wacc

import parsley.Parsley
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc}
import parsley.token.symbol.ImplicitSymbol
import parsley.token.{Lexer, predicate}

object lexer {
  private val keywords: Set[String] = Set("begin", "end", "is", "skip", "read", "free", "return",
    "exit", "print", "println", "if", "then", "else", "fi", "while", "do", "done", "fst", "snd",
    "newpair", "call", "int", "bool", "char", "string", "pair", "true", "false", "null")
  private val operators: Set[String] = Set("+", "-", "*", "/", "len", "ord", "chr", "!", "-", "%",
    ">", ">=", "<", "<=", "==", "!=", "&&", "||")
  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(c => c.isLetter || c == '_'),
      identifierLetter = predicate.Basic(c => c.isLetterOrDigit || c == '_')
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      space = predicate.Basic(c => c.isWhitespace || c == '\t' || c == '\n')
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        literals = Set('\'', '\"', '\\'),
        singleMap = Map(
          '0' -> 0x00, // ?
          'b' -> 0x08,
          't' -> 0x09,
          'n' -> 0x0A,
          'f' -> 0x0C,
          'r' -> 0x0D,
        )
      )
    ),
    symbolDesc = LexicalDesc.plain.symbolDesc.copy(
      hardKeywords = keywords,
      hardOperators = operators
    )
  )
  private val lexer = new Lexer(desc)

  val IDF: Parsley[String] = lexer.lexeme.names.identifier
  val INT: Parsley[Int] = lexer.lexeme.numeric.unsigned.decimal32 // int32, 32-bit signed integer
  val CHR: Parsley[Char] = lexer.lexeme.text.character.ascii
  val STR: Parsley[String] = lexer.lexeme.text.string.ascii

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
