package wacc

import parsley.errors.{ErrorBuilder, tokenextractors}

object TestErrors {
  case class TestError(pos: (Int, Int), lines: TestErrorLines)

  sealed trait TestErrorLines

  case class VanillaError(unexpected: Option[TestErrorItem], expecteds: Set[TestErrorItem], reasons: Set[String]) extends TestErrorLines

  case class SpecialisedError(msgs: Set[String]) extends TestErrorLines

  sealed trait TestErrorItem

  case class TestRaw(item: String) extends TestErrorItem

  case class TestNamed(item: String) extends TestErrorItem

  case object TestEndOfInput extends TestErrorItem

  class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {

    override def format(pos: Position, source: Source, lines: ErrorInfoLines): TestError = TestError(pos, lines)

    type Position = (Int, Int)

    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Unit

    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = TestErrorLines

    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
      VanillaError(unexpected, expected, reasons)

    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs)

    type ExpectedItems = Set[TestErrorItem]

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    type Messages = Set[String]

    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    type UnexpectedLine = Option[TestErrorItem]

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    type ExpectedLine = Set[TestErrorItem]

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    type LineInfo = Unit

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo = ()

    type Item = TestErrorItem
    type Raw = TestRaw
    type Named = TestNamed
    type EndOfInput = TestEndOfInput.type

    override def raw(item: String): Raw = TestRaw(item)

    override def named(item: String): Named = TestNamed(item)

    override val endOfInput: EndOfInput = TestEndOfInput

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int = 0
  }
}
