package wacc

import org.scalatest.flatspec.AnyFlatSpec

class multipleTest extends AnyFlatSpec {

  "multiple semantic errors" should "all fail" in {
    helperFunction.semanticErrAllFail("wacc_examples/invalid/semanticErr/multiple")
  }

}