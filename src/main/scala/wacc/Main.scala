package wacc

import parsley.{Failure, Success}
import wacc.error.WaccErrorBuilder

object Main {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile(args.head)
    val inputList = try source.getLines().toList finally source.close()
    val input = inputList.mkString("\n")
    implicit val eb: WaccErrorBuilder = new WaccErrorBuilder
    parser.parser.parse(input) match {
      case Success(x) =>
        println(s"$input = $x")
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        if (x.check(new SymbolTable(None))(seb)) {
          println("Program is semantically correct")
        } else {
          println("Program is semantically incorrect")
          seb.printAll()
          sys.exit(200)
        }
      case Failure(msg) =>
        println(msg)
        sys.exit(100)
    }
  }
}
