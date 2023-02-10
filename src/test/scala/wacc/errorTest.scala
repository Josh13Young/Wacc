package wacc

import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.TestErrors.TestErrorBuilder

class errorTest extends AnyFlatSpec {

  "program does not have end statement" should "fail and give error message" in {
    val input = "begin skip"
    val output = parseOutput(input)

    val answer = "TestError((1,11),VanillaError(Some(TestEndOfInput),Set(TestNamed(end)),Set()))"

    output shouldBe answer
  }

  "array literals are not first-class expressions" should "fail and give error message" in {
    val input = "begin\n   int[] b = [1, 2, 3] ++ [4] ;\n	  println b[3]\nend"
    val output = parseOutput(input)

    val answer = "TestError((2,24),VanillaError(Some(TestRaw(++ )),Set(TestNamed(end)),Set()))"

    output shouldBe answer
  }

  "missing comment declaration" should "fail and give error message" in {
    val input = "begin\n  oops, this is supposed to be a comment\n  print \"Hello World!\n\"end"
    val output = parseOutput(input)

    val answer = "TestError((2,7),VanillaError(Some(TestRaw(,)),Set(TestNamed(index), TestNamed(assignment)),Set()))"

    output shouldBe answer
  }

  "missing inline comment declaration" should "fail and give error message" in {
    val input = "begin\n  int x = 10 ; can I put in-line comments after the ';'?\n  x = 20 ; #yes, but only if you remember to use the '#' charcter!\n  exit x\nend"
    val output = parseOutput(input)

    val answer = "TestError((2,20),VanillaError(Some(TestRaw(I)),Set(TestNamed(index), TestNamed(assignment)),Set()))"

    output shouldBe answer
  }

  "begin token typo" should "fail and give error message" in {
    val input = "bgn skip end"
    val output = parseOutput(input)

    val answer = "TestError((1,1),VanillaError(Some(TestRaw(bgn s)),Set(TestNamed(begin)),Set()))"

    output shouldBe answer
  }

  "program has multiple begins" should "fail and give error message" in {
    val input = "begin\n  skip\nend\n\nbegin\n  skip\nend"
    val output = parseOutput(input)

    val answer = "TestError((5,1),VanillaError(Some(TestRaw(b)),Set(TestEndOfInput),Set()))"

    output shouldBe answer
  }

  "program has no body" should "fail and give error message" in {
    val input = "begin end"
    val output = parseOutput(input)

    val answer = "TestError((1,7),VanillaError(Some(TestNamed(keyword end)),HashSet(TestNamed(function definition), TestNamed(char), TestNamed(begin), TestNamed(skip), TestNamed(pair), TestNamed(println), TestNamed(string), TestNamed(snd), TestNamed(if statement), TestNamed(fst), TestNamed(read), TestNamed(int), TestNamed(while loop), TestNamed(bool), TestNamed(identifier), TestNamed(free), TestNamed(exit), TestNamed(return), TestNamed(print)),Set()))"

    output shouldBe answer
  }

  "skip token typo" should "fail and give error message" in {
    val input = "begin skp end"
    val output = parseOutput(input)

    val answer = "TestError((1,11),VanillaError(Some(TestRaw(e)),Set(TestNamed(index), TestNamed(assignment)),Set()))"

    output shouldBe answer
  }

  "operator missing first operand" should "fail and give error message" in {
    val input = "begin\n  int b = * 6\nend"
    val output = parseOutput(input)

    val answer = "TestError((2,11),VanillaError(Some(TestRaw(* 6\nend)),HashSet(TestNamed(function call), TestNamed(null), TestNamed(open parenthesis), TestNamed(character literal), TestNamed(snd), TestNamed(string literal), TestNamed(fst), TestNamed(pair literal), TestNamed(unary operator), TestNamed(array literal), TestNamed(boolean literal), TestNamed(integer literal), TestNamed(identifier)),Set()))"

    output shouldBe answer
  }

  "operator missing second operand" should "fail and give error message" in {
    val input = "begin\n  int b = 2 -\nend"
    val output = parseOutput(input)

    val answer = "TestError((3,1),VanillaError(Some(TestNamed(keyword end)),HashSet(TestNamed(null), TestNamed(open parenthesis), TestNamed(character literal), TestNamed(string literal), TestNamed(unary operator), TestNamed(boolean literal), TestNamed(integer literal), TestNamed(identifier)),Set()))"

    output shouldBe answer
  }

  "string concatenation is not valid" should "fail and give error message" in {
    val input = "begin\n  println \"Hello \" ++ \"World!\"\nend"
    val output = parseOutput(input)

    val answer = "TestError((2,22),VanillaError(Some(TestNamed(space)),Set(TestNamed(digit)),Set()))"

    output shouldBe answer
  }

  "missing else clause" should "fail and give error message" in {
    val input = "begin\n  if true\n  then\n    skip\n  fi\nend"
    val output = parseOutput(input)

    val answer = "TestError((5,3),VanillaError(Some(TestRaw(fi\ne)),Set(TestNamed(else)),Set()))"

    output shouldBe answer
  }

  "missing closing fi" should "fail and give error message" in {
    val input = "begin\n  if true\n  then\n    skip\n  else\n    skip\nend"
    val output = parseOutput(input)

    val answer = "TestError((7,1),VanillaError(Some(TestRaw(en)),Set(TestNamed(fi)),Set()))"

    output shouldBe answer
  }

  "missing then clause" should "fail and give error message" in {
    val input = "begin\n  if true\n  else\n    skip\n  fi\nend"
    val output = parseOutput(input)

    val answer = "TestError((3,3),VanillaError(Some(TestRaw(else)),Set(TestNamed(binary operator), TestNamed(then)),Set()))"

    output shouldBe answer
  }

  "if token typo" should "fail and give error message" in {
    val input = "begin\n  ifi true\n  then\n    skip\n  else\n    skip\n  fi\nend"
    val output = parseOutput(input)

    val answer = "TestError((2,7),VanillaError(Some(TestRaw(t)),Set(TestNamed(index), TestNamed(assignment)),Set()))"

    output shouldBe answer
  }

  private def parseOutput(input: String): String = {
    var output = ""
    implicit val eb: TestErrorBuilder = new TestErrorBuilder
    parser.parser.parse(input) match {
      case Success(x) => output = s"$x"
      case Failure(msg) => output = msg.toString
    }
    output
  }

}
