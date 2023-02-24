package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import wacc.helperFunction.{assemblyRunner, getExpectedOutput, getInputList}

import java.io.File

class CodeGenTest extends AnyFlatSpec {

  "andExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andExpr.wacc")
    val inputList = getInputList(file)
    val output = assemblyRunner(inputList)

    output shouldBe getExpectedOutput(inputList)
  }
}
