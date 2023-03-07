package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.helperFunction.assemblyRunFolder

class OptimiseTest extends AnyFlatSpec {

  "array" should "succeed and print correct output for array" in {
    assemblyRunFolder("wacc_examples/valid/array", isOptimise = true)
  }

  "basic" should "succeed and print correct output for basic" in {
    assemblyRunFolder("wacc_examples/valid/basic/exit", isOptimise = true)
    assemblyRunFolder("wacc_examples/valid/basic/skip", isOptimise = true)
  }

  "expressions" should "succeed and print correct output for expressions" in {
    assemblyRunFolder("wacc_examples/valid/expressions", isOptimise = true)
  }

  "function" should "succeed and print correct output for functions" in {
    assemblyRunFolder("wacc_examples/valid/function", isOptimise = true)
  }

  "if" should "succeed and print correct output for if" in {
    assemblyRunFolder("wacc_examples/valid/if", isOptimise = true)
  }

  "IO" should "succeed and print correct output for IO" in {
    assemblyRunFolder("wacc_examples/valid/IO", isOptimise = true)
    assemblyRunFolder("wacc_examples/valid/IO/print", isOptimise = true)
    assemblyRunFolder("wacc_examples/valid/IO/read", isOptimise = true)
  }

  "pairs" should "succeed and print correct output for pairs" in {
    assemblyRunFolder("wacc_examples/valid/pairs", isOptimise = true)
  }

  "runtimeErr" should "succeed and print correct output for runtimeErr" in {
    assemblyRunFolder("wacc_examples/valid/runtimeErr/arrayOutOfBounds", isOptimise = true)
    assemblyRunFolder("wacc_examples/valid/runtimeErr/divideByZero", isOptimise = true)
    assemblyRunFolder("wacc_examples/valid/runtimeErr/integarOverflow", isOptimise = true)
    assemblyRunFolder("wacc_examples/valid/runtimeErr/nullDereference", isOptimise = true)
  }

  "scope" should "succeed and print correct output for scope" in {
    assemblyRunFolder("wacc_examples/valid/scope", isOptimise = true)
  }

  "sequence" should "succeed and print correct output for sequence" in {
    assemblyRunFolder("wacc_examples/valid/sequence", isOptimise = true)
  }

  "variables" should "succeed and print correct output for variables" in {
    assemblyRunFolder("wacc_examples/valid/variables", isOptimise = true)
  }

  "while" should "succeed and print correct output for while" in {
    assemblyRunFolder("wacc_examples/valid/while", isOptimise = true)
  }

}
