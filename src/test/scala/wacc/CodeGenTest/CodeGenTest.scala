package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.helperFunction.assemblyRunFolder

class CodeGenTest extends AnyFlatSpec {

  "array" should "succeed and print correct output for array" in {
    assemblyRunFolder("wacc_examples/valid/array")
  }

  "basic" should "succeed and print correct output for basic" in {
    assemblyRunFolder("wacc_examples/valid/basic/exit")
    assemblyRunFolder("wacc_examples/valid/basic/skip")
  }

  "expressions" should "succeed and print correct output for expressions" in {
    assemblyRunFolder("wacc_examples/valid/expressions")
  }

  "function" should "succeed and print correct output for functions" in {
    assemblyRunFolder("wacc_examples/valid/function")
  }

  "if" should "succeed and print correct output for if" in {
    assemblyRunFolder("wacc_examples/valid/if")
  }

  "IO" should "succeed and print correct output for IO" in {
    assemblyRunFolder("wacc_examples/valid/IO")
    assemblyRunFolder("wacc_examples/valid/IO/print")
    assemblyRunFolder("wacc_examples/valid/IO/read")
  }

  "pairs" should "succeed and print correct output for pairs" in {
    assemblyRunFolder("wacc_examples/valid/pairs")
  }

  "runtimeErr" should "succeed and print correct output for runtimeErr" in {
    assemblyRunFolder("wacc_examples/valid/runtimeErr/arrayOutOfBounds")
    assemblyRunFolder("wacc_examples/valid/runtimeErr/divideByZero")
    assemblyRunFolder("wacc_examples/valid/runtimeErr/integarOverflow")
    assemblyRunFolder("wacc_examples/valid/runtimeErr/nullDereference")
  }

  "scope" should "succeed and print correct output for scope" in {
    assemblyRunFolder("wacc_examples/valid/scope")
  }

  "sequence" should "succeed and print correct output for sequence" in {
    assemblyRunFolder("wacc_examples/valid/sequence")
  }

  "variables" should "succeed and print correct output for variables" in {
    assemblyRunFolder("wacc_examples/valid/variables")
  }

  "while" should "succeed and print correct output for while" in {
    assemblyRunFolder("wacc_examples/valid/while")
  }

}
