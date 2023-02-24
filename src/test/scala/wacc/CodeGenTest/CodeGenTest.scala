package wacc

import org.scalatest.flatspec._
import org.scalatest.matchers.must.Matchers.matchPattern
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.frontend.{SymbolTable, parser}

import java.io.{File, PrintWriter}
import scala.language.postfixOps
import sys.process._

class CodeGenTest extends AnyFlatSpec {

  "andExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\ntrue\nfalse\n"
  }

  "andOverOrExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andOverOrExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "boolCalc" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/boolCalc.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\n"
  }

  "boolExpr1" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/boolExpr1.wacc")
    val output = assemblyRunner(file)

    output shouldBe "Correct\n"
  }

  "charComparisonExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/charComparisonExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\ntrue\ntrue\ntrue\nfalse\nfalse\n"
  }

  "divExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/divExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "1\n"
  }

  "equalsExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\nfalse\ntrue\n"
  }

  "equalsOverAnd" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsOverAnd.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\ntrue\n"
  }

  "equalsOverBool" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsOverBool.wacc")
    val output = assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "equalsOverOr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsOverOr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "greaterEqExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/greaterEqExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\ntrue\ntrue\n"
  }

  "greaterExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/greaterExpr.wacc")
    val output = assemblyRunner(file)

    output shouldBe "false\ntrue\n"
  }

  "intCalc" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/intCalc.wacc")
    val output = assemblyRunner(file)

    output shouldBe "72\n"
  }

  "intExpr1" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/intExpr1.wacc")
    val output = assemblyRunner(file)

    output shouldBe "Correct\n"
  }

  private def assemblyRunner(file: File): String = {
    val source = scala.io.Source.fromFile(file)
    val inputList = try source.getLines().toList finally source.close()
    val input = inputList.mkString("\n")
    parser.parser.parse(input) match {
      case Success(x) =>
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        val st = new SymbolTable(None)
        if (x.check(st)(seb)) {
          CodeGenerator.st = st
          val code = generateString(generate(x))
          val pw = new PrintWriter("temp.s")
          pw.write(code)
          pw.close()

          val output = ("sh test.sh" !!)
          output
        } else {
          seb.printAll()
          if (seb.isSemantic) {
            sys.exit(200)
          } else {
            sys.exit(100)
          }
        }

      case Failure(msg) =>
        // Should throw error if parser failed
        return "";
    }
  }

}
