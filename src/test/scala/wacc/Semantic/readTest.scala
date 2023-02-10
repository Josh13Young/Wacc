package wacc

import org.scalatest.flatspec.AnyFlatSpec

class readTest extends AnyFlatSpec {

  "read semantic errors" should "all fail" in {
    helperFunction.validCasesAllSucceed("wacc_examples/invalid/semanticErr/read")
  }

}