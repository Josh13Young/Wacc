package wacc

import org.scalatest.flatspec.AnyFlatSpec

class exitTest extends AnyFlatSpec {

  "exit semantic errors" should "all fail" in {
    helperFunction.validCasesAllSucceed("wacc_examples/invalid/semanticErr/exit")
  }
  
}