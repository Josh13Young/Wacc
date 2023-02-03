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

  "valid/basic/exit/exit-1" should "succeed and return exit -1" in {
    val input = "begin exit -1 end"
    val output = parseOutput(input)   

    val answer = "Program(List(),List(Exit(Neg(IntLiter(1)))))"

    output shouldEqual answer;
  }

  "valid/basic/exit/exitBasic" should "succeed and return exit 7" in {
    val input = "begin exit 7 end"
    val output = parseOutput(input)   

    val answer = "Program(List(),List(Exit(IntLiter(7))))"

    output shouldEqual answer;
  }

  "valid/basic/skip/skip" should "succeed and return skip" in {
    val input = "begin skip end"
    val output = parseOutput(input)   

    val answer = "Program(List(),List(Skip()))"

    output shouldEqual answer;
  }

  "valid/IO/print/print" should "succeed and return print(Hello World!)" in {
    val input = "begin print \"Hello World!\" end"
    val output = parseOutput(input)

    println(output)

    val answer = "Program(List(),List(Print(StrLiter(Hello World!))))"

    output shouldEqual answer;
  }

  "valid/IO/print/printBool" should "succeed and return print(true is) Println(BoolLiter(true))..." in {
    val input = "begin print \"True is \" ; println true ; print \"False is \" ; println false end"
    val output = parseOutput(input)

    println(output)

    val answer = "Program(List(),List(Print(StrLiter(True is )), Println(BoolLiter(true)), Print(StrLiter(False is )), Println(BoolLiter(false))))"

    output shouldEqual answer;
  }

  "valid/IO/print/printChar" should "succeed and return print(true is) Println(BoolLiter(true))..." in {
    val input = "begin print \"A simple character example is \" ; println 'f' end"
    val output = parseOutput(input)

    println(output)

    val answer = "Program(List(),List(Print(StrLiter(A simple character example is )), Println(CharLiter(f))))"

    output shouldEqual answer;
  }

  "comment" should "succeed and return comment" in {
    val input = "begin skip # comment\n end"
    val output = parseOutput(input)

    println(output)

    val answer = "Program(List(),List(Skip()))"

    output shouldEqual answer;
  }

  "boolean and symbols" should "succeed" in {
    val input = "begin\n  bool a = true ;\n  bool b = false ;\n  println a && b ;\n  println a && true ;\n  println b && false\nend"

    val output = parseOutput(input)

    println(output)

    val answer = "Program(List(),List(AssignNew(BoolType(),Ident(a),BoolLiter(true)), AssignNew(BoolType(),Ident(b),BoolLiter(false)), Println(And(Ident(a),Ident(b))), Println(And(Ident(a),BoolLiter(true))), Println(And(Ident(b),BoolLiter(false)))))"

    output shouldEqual answer;
  }

  "more boolean symbols" should "succeed" in {
    val input = "begin\n  bool b = ! ( ( true && false) || (true && false) );\n  if b == true then\n    println \"Correct\"\n  else\n    println \"Wrong\"\n  fi\nend"

    val output = parseOutput(input)

    println(output)

    val answer = "Program(List(),List(AssignNew(BoolType(),Ident(b),Not(Or(And(BoolLiter(true),BoolLiter(false)),And(BoolLiter(true),BoolLiter(false))))), If(EQ(Ident(b),BoolLiter(true)),List(Println(StrLiter(Correct))),List(Println(StrLiter(Wrong))))))"

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
