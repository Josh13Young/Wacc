package wacc.error

import parsley.errors.{ErrorBuilder, Token}
import parsley.errors.tokenextractors.MatchParserDemand
import wacc.error.Error._

class WaccErrorBuilder extends ErrorBuilder[WaccError] {
  override def format(pos: Position, source: Source, lines: ErrorInfoLines): WaccError =
    WaccError(pos, lines)

  override type Position = (Int, Int)
  override type Source = Option[String]

  override def pos(line: Int, col: Int): Position = (line, col)

  override def source(sourceName: Option[String]): Source = sourceName

  override type ErrorInfoLines = WaccErrorLines

  override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
    WaccVanillaError(unexpected, expected, reasons, line)

  override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines =
    WaccSpecialisedError(msgs, line)

  override type ExpectedItems = Set[WaccErrorItem]
  override type Messages = List[String]

  override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

  override def combineMessages(alts: Seq[Message]): Messages = alts.toList

  override type UnexpectedLine = Option[WaccErrorItem]
  override type ExpectedLine = Set[WaccErrorItem]
  override type Message = String
  override type LineInfo = WaccLineInfo

  override def unexpected(item: Option[Item]): UnexpectedLine = item

  override def expected(alts: ExpectedItems): ExpectedLine = alts

  override def reason(reason: String): Message = reason

  override def message(msg: String): Message = msg

  override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo =
    WaccLineInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth)

  override val numLinesBefore: Int = 1
  override val numLinesAfter: Int = 1
  override type Item = WaccErrorItem
  override type Raw = WaccErrorRaw
  override type Named = WaccErrorNamed
  override type EndOfInput = WaccErrorEndOfInput.type

  override def raw(item: String): Raw = WaccErrorRaw(item)

  override def named(item: String): WaccErrorNamed = WaccErrorNamed(item)

  override val endOfInput: EndOfInput = WaccErrorEndOfInput

  override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token =
    MatchParserDemand.unexpectedToken(cs, amountOfInputParserWanted)
}


