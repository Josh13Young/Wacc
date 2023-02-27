package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.helperFunction.assemblyRunFolder

class CodeGenTest extends AnyFlatSpec {

  "advanced" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/advanced")
  }

//  "array" should "succeed and print correct output" in {
//    assemblyRunFolder("wacc_examples/valid/array")
//  }

  "basic" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/basic")
  }

  "expressions" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/expressions")
  }

  "function" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/function")
  }

  "if" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/if")
  }

  "IO" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/IO")
  }

  "pairs" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/pairs")
  }

  "runtimeErr" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/runtimeErr")
  }

  "scope" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/scope")
  }

  "sequence" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/sequence")
  }

  "variables" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/variables")
  }

  "while" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/while")
  }
}
