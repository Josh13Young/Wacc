package wacc

import org.scalatest.flatspec.AnyFlatSpec

class functionTest extends AnyFlatSpec {
  // TODO: fix this by using Errors
//  "function syntax errors" should "all fail" in {
//    helperFunction.syntaxErrAllFail("wacc_examples/invalid/syntaxErr/function")
//  }

  "function nested valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/function/nested_functions")
  }

  "function simple valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/function/nested_functions")
  }
}
