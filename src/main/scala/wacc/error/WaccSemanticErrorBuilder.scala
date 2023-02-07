package wacc.error

import wacc.error.Error.WaccError

import scala.collection.mutable

object WaccSemanticErrorBuilder {
  def apply(pos: (Int, Int), msg: String)(implicit errors:SemanticError): Unit = {
    val line = pos._1 - 1
    val linesBefore = if (line - 2 >= 0) errors.program.slice(line - 2, line) else errors.program.slice(0, line)
    val linesAfter = if (line + 2 < errors.program.length) errors.program.slice(line + 1, line + 2) else errors.program.slice(line + 1, errors.program.length)
    val error = WaccError(pos, Error.WaccSemanticError(msg, Error.WaccLineInfo(errors.program(line), linesBefore, linesAfter, pos._2 - 1, 1)))
    errors.add(error)
  }

  class SemanticError {
    var program: List[String] = List()
    var errors: mutable.Set[WaccError] = mutable.Set()
    def add (error: WaccError): Unit = {
      errors += error
    }
  }

  case object UnaryOperatorError {
    def apply(pos: (Int,Int), operator: String, t: String)(implicit errors:SemanticError): Unit = {
      WaccSemanticErrorBuilder(pos, s"Unary operator $operator can only be applied to $t")
    }
  }

  case object BinaryOperatorError {
    def apply(pos:(Int, Int), operator: String, t: Set[String])(implicit errors:SemanticError): Unit = {
      val types = t.mkString(", ")
      WaccSemanticErrorBuilder(pos, s"Binary operator $operator can only be applied to $types")
    }
  }

  case object TypeMismatchError {
    def apply(pos:(Int, Int), t1: String, t2: String)(implicit errors:SemanticError): Unit = {
      WaccSemanticErrorBuilder(pos, s"Type mismatch: $t1 and $t2")
    }
  }

  case object IdentAlreadyDefinedError {
    def apply(pos:(Int, Int), name: String)(implicit errors:SemanticError): Unit = {
      WaccSemanticErrorBuilder(pos, s"Identifier $name is already defined")
    }
  }
}
