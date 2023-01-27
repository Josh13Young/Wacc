package wacc

import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.must.Matchers.{a, not}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import parsley.Parsley._
import parsley.Success
import parsley.implicits.character.{charLift, stringLift}

class ParserTest extends AnyFlatSpec {

  "example test..." should "fail if no input" in {
    'a'.parse("") should not be a [Success[_]]
  }


}
