package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.helperFunction.assemblyRunFolder

class CodeGenTest extends AnyFlatSpec {

  "Expr" should "succeed and print correct output" in {
    assemblyRunFolder("wacc_examples/valid/expressions")
  }
}
