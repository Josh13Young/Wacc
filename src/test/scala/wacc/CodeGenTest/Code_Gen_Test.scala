package wacc

import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.frontend.{SymbolTable, parser}

import java.io.PrintWriter
import sys.process._

class Code_Gen_Test extends AnyFlatSpec {

  "test" should "succeed" in {

    val file = "wacc_examples/valid/expressions/addExpr.wacc"

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
  }

}
