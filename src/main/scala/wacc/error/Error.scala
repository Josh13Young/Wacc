package wacc.error

object Error {
  case class WaccError(pos: (Int, Int), lines: WaccErrorLines) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append(s"Error at line ${pos._1}, column ${pos._2}:\n")
      sb.append(lines.toString)
      sb.toString()
    }
  }

  sealed trait WaccErrorLines

  case class WaccVanillaError(unexpected: Option[WaccErrorItem], expected: Set[WaccErrorItem], reasons: List[String], info: WaccLineInfo) extends WaccErrorLines {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("Syntax error\n")
      sb.append("Unexpected \"" + unexpected.get + "\".\n")
      if (expected.nonEmpty)
        sb.append("Expected one of: " + expected.mkString(", ") + ".\n")
      if (reasons.nonEmpty)
        sb.append("Reasons: " + reasons.mkString(", ") + ".\n")
      sb.append(info)
      sb.toString()
    }
  }

  case class WaccSpecialisedError(msgs: List[String], info: WaccLineInfo) extends WaccErrorLines

  case class WaccSemanticError(msg: String, info: WaccLineInfo) extends WaccErrorLines {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("Semantic error\n")
      sb.append(msg + ".\n")
      sb.append(info)
      sb.toString()
    }
  }

  sealed trait WaccErrorItem

  case class WaccErrorRaw(item: String) extends WaccErrorItem {
    override def toString: String = item
  }

  case class WaccErrorNamed(item: String) extends WaccErrorItem {
    override def toString: String = item
  }

  case object WaccErrorEndOfInput extends WaccErrorItem {
    override def toString: String = "End of file"
  }

  case class WaccLineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("----Code Snippet----\n")
      for (l <- linesBefore) {
        sb.append("  " + l + "\n")
      }
      sb.append("> " + line + "\n")
      for (i <- 0 until errorPointsAt + 2) {
        sb.append(" ")
      }
      for (i <- 0 until errorWidth) {
        sb.append("^")
      }
      sb.append("\n")
      for (l <- linesAfter) {
        sb.append("  " + l + "\n")
      }
      sb.append("--------------------\n")
      sb.toString()
    }
  }
}

