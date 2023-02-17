import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.frontend.parser
import wacc.helperFunction
import sys.process._

class CodeGenTest extends AnyFlatSpec {

  "test" should "succeed" in {

    val file = "/homes/jy2221/y2/wacc/WACC_32/wacc_examples/valid/expressions/addExpr.wacc"

    val source = scala.io.Source.fromFile(file)
    val inputList = try source.getLines().toList finally source.close()
    val input = inputList.mkString("\n")
    parser.parser.parse(input) match {
      case Success(x) =>
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        val st = new SymbolTable(None)
        if (x.check(st)(seb)) {
          println("Program is semantically correct")
          CodeGenerator.st = st
          val code = generateString(generate(x))
          val fileName = args.head.split("/").last.split("\\.").head
          val pw = new PrintWriter("temp.s")
          pw.write(code)
          pw.close()

          val output = "./test.sh" !!
          println(output)
          1 shouldBe 1
        }

      case Failure(msg) =>
        // Should throw error if parser failed
        true shouldBe false
    }


    output shouldEqual answer
  }

}
