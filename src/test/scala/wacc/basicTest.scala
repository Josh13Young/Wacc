package wacc

import org.scalatest.flatspec.AnyFlatSpec

class basicTest extends AnyFlatSpec {
  "expression syntax errors" should "all fail" in {
    helperFunction.syntaxErrAllFail("wacc_examples/invalid/syntaxErr/expressions")
  }

  "expression valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/expressions")
  }

  "basic syntax errors" should "all fail" in {
    helperFunction.syntaxErrAllFail("wacc_examples/invalid/syntaxErr/basic")
  }

  "basic valid examples" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/basic/exit")
    helperFunction.validCasesAllSucceed("wacc_examples/valid/basic/skip")
  }

  "IO tests" should "all succeed" in {
    helperFunction.validCasesAllSucceed("wacc_examples/valid/IO")
    helperFunction.validCasesAllSucceed("wacc_examples/valid/IO/read")
  }

}
