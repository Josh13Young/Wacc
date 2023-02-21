package wacc

import org.scalatest.flatspec._
import org.scalatest.matchers.must.Matchers.matchPattern
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.frontend.{SymbolTable, parser}

import java.io.{File, PrintWriter}
import scala.language.postfixOps
import sys.process._

class Code_Gen_Test extends AnyFlatSpec {

  "test" should "succeed" in {
    val testlist = "wacc_examples/valid/array"
    val files = getListOfFiles(testlist)


    println(0)
    for (file <- files) {
      println(file)
      println(1)
    }


    //val output = helper(file)
    //println(output)
    1 shouldBe(1)

  }

  private def helper(file: String): String = {
    var output = ""
    val source = scala.io.Source.fromFile(file)
    val inputList = try source.getLines().toList finally source.close()
    val input = inputList.mkString("\n")
    parser.parser.parse(input) match {
      case Success(x) =>
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        val st = new SymbolTable(None)
        if (x.check(st)(seb)) {
          println("Program is semantically correct")
          CodeGenerator.st = st
          val code = generateString(generate(x))
          val pw = new PrintWriter("temp.s")
          pw.write(code)
          pw.close()

          val output = ("./test.sh" !!)
          output
        } else {
          seb.printAll()
          if (seb.isSemantic) {
            sys.exit(200)
          } else {
            sys.exit(100)
          }
        }

      case Failure(msg) =>
        // Should throw error if parser failed
        return "";
    }
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def getInput(file: File): String = {
    val source = scala.io.Source.fromFile(file)
    val input = try source.getLines().toList.mkString("\n") finally source.close()
    input
  }

}
