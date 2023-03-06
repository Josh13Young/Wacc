package wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.helperFunction.assemblyRunFolder

class StLibTest extends AnyFlatSpec {

  "stlib" should "succeed and print correct output for stlib" in {
    assemblyRunFolder("standard_library/stlibTestExample")
  }

}
