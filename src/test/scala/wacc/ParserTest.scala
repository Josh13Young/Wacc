package wacc

import org.scalatest._
import flatspec._
import org.scalatest.matchers.must.Matchers.{not, be, equal, eq}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import parsley.Parsley._
import parsley.{Failure, Success}
import parsley.Success
import parsley.implicits.character.{charLift, stringLift}

class ParserTest extends AnyFlatSpec {

  "begin return 3 end" should "succeed and return IntLiter(3)" in {
    val input = "begin return 3 end"
    val output = parseOutput(input)   

    val answer = "Program(List(),List(Return(IntLiter(3))))"

    output shouldEqual answer;
  }

  private def parseOutput(input: String) : String = {
    var output = ""
    parser.parser.parse(input) match {
            case Success(x) => output = (s"$x")
            case Failure(msg) => output = (msg)
        }
    return output
  }

}
