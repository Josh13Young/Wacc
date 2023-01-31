package wacc

import org.scalatest.flatspec.AnyFlatSpec

class pairTest extends AnyFlatSpec {

  "pairs syntax errors" should "all fail" in {
    helperFunction.syntaxErrAllFail("wacc_examples/invalid/syntaxErr/pairs")
  }

  "pairs valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/pairs")
  }

}
