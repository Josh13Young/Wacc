package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import wacc.helperFunction.{assemblyRunner, getExpectedExitValue, getExpectedOutput, getInputList}

import java.io.File

class CodeGenTest extends AnyFlatSpec {

  "Expr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andExpr.wacc")
    val inputList = getInputList(file)
    val output = assemblyRunner(inputList)
    val expectedExitValue = getExpectedExitValue(inputList)
    (expectedExitValue, output) shouldBe getExpectedOutput(inputList)
  }
}
