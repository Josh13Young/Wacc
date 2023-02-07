package wacc.error

import wacc.error.Error.{WaccError, WaccLineInfo, WaccSemanticError}

import scala.collection.mutable

object WaccSemanticErrorBuilder {
  def apply(pos: (Int, Int), msg: String)(implicit errors: SemanticError): Unit = {
    val line = pos._1 - 1
    val linesBefore = if (line - 2 >= 0) errors.program.slice(line - 2, line) else errors.program.slice(0, line)
    val linesAfter = if (line + 2 < errors.program.length) errors.program.slice(line + 1, line + 2) else errors.program.slice(line + 1, errors.program.length)
    val error = WaccError(pos, WaccSemanticError(msg, WaccLineInfo(errors.program(line), linesBefore, linesAfter, pos._2 - 1, 1)))
    errors.add(error)
  }

  class SemanticError {
    var program: List[String] = List()
    var errors: mutable.ListBuffer[WaccError] = mutable.ListBuffer()

    def add(error: WaccError): Unit = {
      errors += error
    }

    def printAll(): Unit = {
      for (e <- errors) {
        println(e)
      }
    }
  }

  case object UnaryOperatorError {
    def apply(pos: (Int, Int), operator: String, t: String, exprType: String)(implicit errors: SemanticError): Unit = {
      if (exprType == "void")
        WaccSemanticErrorBuilder(pos, s"\"$operator\" failed.\nThe given type is invalid, encountered a semantic error before?")
      else
        WaccSemanticErrorBuilder(pos, s"Unary operator \"$operator\" can only be applied to $t, type $exprType given")
    }
  }

  case object BinaryOperatorError {
    def apply(pos: (Int, Int), operator: String, t: Set[String], t1: String, t2: String)(implicit errors: SemanticError): Unit = {
      (t1, t2) match {
        case ("void", "void") => WaccSemanticErrorBuilder(pos, s"\"$operator\" failed.\nBoth types are invalid, encountered a semantic error before?")
        case ("void", _) => WaccSemanticErrorBuilder(pos, s"\"$operator\" failed.\nThe left type is invalid, encountered a semantic error before?")
        case (_, "void") => WaccSemanticErrorBuilder(pos, s"\"$operator\" failed.\nThe right type is invalid, encountered a semantic error before?")
        case _ =>
          val types = t.mkString(", ")
          WaccSemanticErrorBuilder(pos, s"Binary operator \"$operator\" can only be applied to $types, types $t1 and $t2 given")
      }
    }
  }

  case object TypeMismatchError {
    def apply(pos: (Int, Int), t1: String, t2: String)(implicit errors: SemanticError): Unit = {
      (t1, t2) match {
        case ("void", "void") => WaccSemanticErrorBuilder(pos, s"Both types are invalid, encountered a semantic error before?")
        case ("void", _) => WaccSemanticErrorBuilder(pos, s"The required type is invalid, encountered a semantic error before?")
        case (_, "void") => WaccSemanticErrorBuilder(pos, s"The given type is invalid, encountered a semantic error before?")
        case _ => WaccSemanticErrorBuilder(pos, s"Type mismatch.\nGiven: $t2, Required: $t1")
      }
    }
  }

  case object IdentAlreadyDefinedError {
    def apply(pos: (Int, Int), name: String)(implicit errors: SemanticError): Unit = {
      WaccSemanticErrorBuilder(pos, s"Identifier $name is already defined")
    }
  }

  case object CondBoolError {
    def apply(pos: (Int, Int), name: String)(implicit errors: SemanticError): Unit = {
      if (name.equals("void"))
        WaccSemanticErrorBuilder(pos, s"The given type for condition is invalid, encountered a semantic error before?")
      else
        WaccSemanticErrorBuilder(pos, s"Condition must be type of bool, type $name given")
    }
  }

  case object StatError {
    def apply(pos: (Int, Int), command: String, types: Set[String], t: String)(implicit errors: SemanticError): Unit = {
      if (t.equals("void"))
        WaccSemanticErrorBuilder(pos, s"\"$command\" failed.\nThe given type is invalid, encountered a semantic error before?")
      else {
        val typesStr = types.mkString(", ")
        WaccSemanticErrorBuilder(pos, s" \"$command\" can only be applied to $typesStr, but a $t is provided")
      }
    }
  }
}
