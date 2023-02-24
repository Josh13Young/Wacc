package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import  java.io.File

class CodeGenTest extends AnyFlatSpec {

  "andExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    println(output)
    output shouldBe "false\ntrue\nfalse\n"
  }

  "andOverOrExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andOverOrExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    println(output)
    output shouldBe "true\nfalse\n"
  }

}
