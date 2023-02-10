package wacc

import org.scalatest.flatspec.AnyFlatSpec

class ioTest extends AnyFlatSpec {

  "io semantic errors" should "all fail" in {
    helperFunction.validCasesAllSucceed("wacc_examples/invalid/semanticErr/IO")
  }
  
}