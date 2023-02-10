package wacc

import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}

class ParserTest extends AnyFlatSpec {

  "valid/basic/exit/exit-1" should "succeed and return exit -1" in {
    val input = "begin exit -1 end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Exit(IntLiter(-1))))"

    output shouldEqual answer
  }

  "valid/basic/exit/exitBasic" should "succeed and return exit 7" in {
    val input = "begin exit 7 end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Exit(IntLiter(7))))"

    output shouldEqual answer
  }

  "valid/basic/skip/skip" should "succeed and return skip" in {
    val input = "begin skip end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Skip()))"

    output shouldEqual answer
  }

  "valid/IO/print/print" should "succeed and return print(Hello World!)" in {
    val input = "begin print \"Hello World!\" end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Print(StrLiter(Hello World!))))"

    output shouldEqual answer
  }

  "valid/IO/print/printBool" should "succeed and return print(true is) Println(BoolLiter(true))..." in {
    val input = "begin print \"True is \" ; println true ; print \"False is \" ; println false end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Print(StrLiter(True is )), Println(BoolLiter(true)), Print(StrLiter(False is )), Println(BoolLiter(false))))"

    output shouldEqual answer
  }

  "valid/IO/print/printChar" should "succeed and return print(true is) Println(BoolLiter(true))..." in {
    val input = "begin print \"A simple character example is \" ; println 'f' end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Print(StrLiter(A simple character example is )), Println(CharLiter(f))))"

    output shouldEqual answer
  }

  "comment" should "succeed and return comment" in {
    val input = "begin skip # comment\n end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Skip()))"

    output shouldEqual answer
  }

  "boolean and symbols" should "succeed" in {
    val input = "begin\n  bool a = true ;\n  bool b = false ;\n  println a && b ;\n  println a && true ;\n  println b && false\nend"
    val output = parseOutput(input)

    val answer = "Program(List(),List(AssignNew(BoolType(),Ident(a),BoolLiter(true)), AssignNew(BoolType(),Ident(b),BoolLiter(false)), Println(And(Ident(a),Ident(b))), Println(And(Ident(a),BoolLiter(true))), Println(And(Ident(b),BoolLiter(false)))))"

    output shouldEqual answer
  }

  "valid/IO/print/printEscChar" should "succeed and return print(true is) Println(BoolLiter(true))..." in {
    val input = "begin print \"An escaped character example is \" ; println '\\\"' end"
    val output = parseOutput(input)

    val answer = "Program(List(),List(Print(StrLiter(An escaped character example is )), Println(CharLiter(\"))))"

    output shouldEqual answer
  }

  "all compare symbols" should "succeed" in {
    val input = "begin\n  char c1 = 'a' ;\n  char c2 = 'z' ;\n  println c1 == c2 ;\n  println c1 != c2 ;\n  println c1 < c2 ;\n  println c1 <= c2 ;\n  println c1 > c2 ;\n  println c1 >= c2\nend"
    val output = parseOutput(input)

    val answer = "Program(List(),List(AssignNew(CharType(),Ident(c1),CharLiter(a)), AssignNew(CharType(),Ident(c2),CharLiter(z)), Println(EQ(Ident(c1),Ident(c2))), Println(NEQ(Ident(c1),Ident(c2))), Println(LT(Ident(c1),Ident(c2))), Println(LTE(Ident(c1),Ident(c2))), Println(GT(Ident(c1),Ident(c2))), Println(GTE(Ident(c1),Ident(c2)))))"

    output shouldEqual answer
  }

  "div sign" should "succeed" in {
    val input = "begin\n  int x = 5 ;\n  int y = 3 ;\n  println x / y\nend"
    val output = parseOutput(input)

    val answer = "Program(List(),List(AssignNew(IntType(),Ident(x),IntLiter(5)), AssignNew(IntType(),Ident(y),IntLiter(3)), Println(Div(Ident(x),Ident(y)))))"

    output shouldEqual answer
  }

  "mod sign" should "succeed" in {
    val input = "begin\n  int x = 5 ;\n  int y = 3 ;\n  println x % y\nend"
    val output = parseOutput(input)

    val answer = "Program(List(),List(AssignNew(IntType(),Ident(x),IntLiter(5)), AssignNew(IntType(),Ident(y),IntLiter(3)), Println(Mod(Ident(x),Ident(y)))))"

    output shouldEqual answer
  }

  "simple function" should "succeed" in {
    val input = "begin\n  int f() is\n    return 0\n  end\n  int x = call f() ;\n  println x\nend"
    val output = parseOutput(input)

    val answer = "Program(List(Func(IntType(),Ident(f),List(),List(Return(IntLiter(0))))),List(AssignNew(IntType(),Ident(x),Call(Ident(f),List())), Println(Ident(x))))"

    output shouldEqual answer
  }

  "nested function" should "succeed" in {
    val input = "begin\n  int f(int x) is\n    if x == 0 then\n      skip\n    else\n      int i = x ;\n      while i > 0 do\n        print \"-\" ;\n        i = i - 1\n      done ;\n      println \"\" ;\n      int s = call f(x - 1)\n    fi ;\n    return 0\n  end\n\n  println \"Please enter the size of the triangle to print:\" ;\n  int x = 0;\n\n  read x;\n  int s = call f(x)\nend"
    val output = parseOutput(input)

    val answer = "Program(List(Func(IntType(),Ident(f),List(Param(IntType(),Ident(x))),List(If(EQ(Ident(x),IntLiter(0)),List(Skip()),List(AssignNew(IntType(),Ident(i),Ident(x)), While(GT(Ident(i),IntLiter(0)),List(Print(StrLiter(-)), Assign(Ident(i),Sub(Ident(i),IntLiter(1))))), Println(StrLiter()), AssignNew(IntType(),Ident(s),Call(Ident(f),List(Sub(Ident(x),IntLiter(1))))))), Return(IntLiter(0))))),List(Println(StrLiter(Please enter the size of the triangle to print:)), AssignNew(IntType(),Ident(x),IntLiter(0)), Read(Ident(x)), AssignNew(IntType(),Ident(s),Call(Ident(f),List(Ident(x))))))"

    output shouldEqual answer
  }

  private def parseOutput(input: String): String = {
    var output = ""
    parser.parser.parse(input) match {
      case Success(x) => output = s"$x"
      case Failure(msg) => output = msg
    }
    output
  }

}
