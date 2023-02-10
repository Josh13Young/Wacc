package wacc

import org.scalatest.flatspec.AnyFlatSpec

class IOTest extends AnyFlatSpec {

  "io semantic errors" should "all fail" in {
    helperFunction.semanticErrAllFail("wacc_examples/invalid/semanticErr/IO")
  }

}