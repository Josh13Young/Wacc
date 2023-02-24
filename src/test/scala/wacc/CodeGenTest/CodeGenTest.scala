package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import  java.io.File

class CodeGenTest extends AnyFlatSpec {

  "andExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\ntrue\nfalse\n"
  }

  "andOverOrExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/andOverOrExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "boolCalc" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/boolCalc.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\n"
  }

  "boolExpr1" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/boolExpr1.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "Correct\n"
  }

  "charComparisonExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/charComparisonExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\ntrue\ntrue\ntrue\nfalse\nfalse\n"
  }

  "divExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/divExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "1\n"
  }

  "equalsExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\nfalse\ntrue\n"
  }

  "equalsOverAnd" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsOverAnd.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\ntrue\n"
  }

  "equalsOverBool" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsOverBool.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "equalsOverOr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/equalsOverOr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "greaterEqExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/greaterEqExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\ntrue\ntrue\n"
  }

  "greaterExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/greaterExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "false\ntrue\n"
  }

  "intCalc" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/intCalc.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "72\n"
  }

  "intExpr1" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/intExpr1.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "Correct\n"
  }

  "lessCharExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/lessCharExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

  "lessEqExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/lessEqExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "true\nfalse\ntrue\n"
  }

  "lessExpr" should "succeed and print correct output" in {
    val file = new File("wacc_examples/valid/expressions/lessExpr.wacc")
    val output = helperFunction.assemblyRunner(file)

    output shouldBe "true\nfalse\n"
  }

}
