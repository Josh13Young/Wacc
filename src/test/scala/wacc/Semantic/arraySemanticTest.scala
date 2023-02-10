package wacc

import org.scalatest.flatspec.AnyFlatSpec

class arraySemanticTest extends AnyFlatSpec {

  "array semantic errors" should "all fail" in {
    helperFunction.semanticErrAllFail("wacc_examples/invalid/semanticErr/IO")
  }

}
