package wacc

import org.scalatest.flatspec.AnyFlatSpec

class arrayTest extends AnyFlatSpec {

  "array syntax errors" should "all fail" in {
    helperFunction.syntaxErrAllFail("wacc_examples/invalid/syntaxErr/array")
  }

  "expression valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/array")
  }

}
