package wacc

import parsley.{Failure, Success}
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.error.WaccErrorBuilder
import wacc.frontend.{SymbolTable, parser}

import java.io.PrintWriter

object Main {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile(args.head)
    val inputList = try source.getLines().toList finally source.close()
    val input = inputList.mkString("\n")
    implicit val eb: WaccErrorBuilder = new WaccErrorBuilder
    parser.parser.parse(input) match {
      case Success(x) =>
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        if (x.check(new SymbolTable(None))(seb)) {
          println("Program is semantically correct")
          val code = generateString(generate(x))
          println(code)
          val fileName = args.head.split("/").last.split("\\.").head
          println(fileName) // generate fileName.s
          val pw = new PrintWriter("out.s")
          pw.write(code)
          pw.close()
        } else {
          seb.printAll()
          if (seb.isSemantic) {
            sys.exit(200)
          } else {
            sys.exit(100)
          }
        }
      case Failure(msg) =>
        println(msg)
        sys.exit(100)
    }
  }
}
