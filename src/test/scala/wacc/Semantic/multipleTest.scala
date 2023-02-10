package wacc

import org.scalatest.flatspec.AnyFlatSpec

class multipleTest extends AnyFlatSpec {

  "multiple semantic errors" should "all fail" in {
    helperFunction.validCasesAllSucceed("wacc_examples/invalid/semanticErr/multiple")
  }

}