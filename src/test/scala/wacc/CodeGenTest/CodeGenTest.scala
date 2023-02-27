package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.helperFunction.assemblyRunFolder

class CodeGenTest extends AnyFlatSpec {

  "advanced" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/advanced")
  }

  "array" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/array")
  }

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
}
