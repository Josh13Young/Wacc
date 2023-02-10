package wacc

import org.scalatest.flatspec.AnyFlatSpec

class functionTest extends AnyFlatSpec {

  "function nested valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/function/nested_functions")
  }

  "function simple valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/function/nested_functions")
  }

}
