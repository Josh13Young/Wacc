package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.matchPattern
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.frontend.{SymbolTable, parser}

import java.io.{File, PrintWriter}
import scala.sys.process._

object helperFunction extends AnyFlatSpec {

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

  def syntaxErrAllFail(dir: String): Unit = {
    val files = helperFunction.getListOfFiles(dir)
    for (file <- files) {
      parser.parser.parse(helperFunction.getInput(file)) should matchPattern { case Failure(_) => }
    }
  }

  def validCasesAllSucceed(dir: String): Unit = {
    val files = helperFunction.getListOfFiles(dir)
    for (file <- files) {
      parser.parser.parse(helperFunction.getInput(file)) should matchPattern { case Success(_) => }
    }
  }

  def semanticErrAllFail(dir: String): Unit = {

    val files = helperFunction.getListOfFiles(dir)
    for (file <- files) {
      val source = scala.io.Source.fromFile(file)
      val inputList = try source.getLines().toList finally source.close()
      val input = inputList.mkString("\n")
      parser.parser.parse(input) match {
        case Success(x) =>
          val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
          seb.program = inputList
          if (x.check(new SymbolTable(None))(seb)) {
            // "Program is semantically correct"
          } else {
            seb.isSemantic shouldBe true
          }
        case Failure(msg) =>
          // Should throw error if parser failed
          true shouldBe false
      }
    }
  }

  def getInputList(file: File): List[String] = {
    val source = scala.io.Source.fromFile(file)
    val inputList = try source.getLines().toList finally source.close()
    inputList
  }

  def getExpectedOutput(inputList: List[String]): List[String] = {
    inputList.dropWhile(!_.startsWith("# Output:")).drop(1).takeWhile(_.nonEmpty).map(_.drop(2))
  }

  def assemblyRunner(inputList: List[String]): String = {
    val input = inputList.mkString("\n")
    parser.parser.parse(input) match {
      case Success(x) =>
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        val st = new SymbolTable(None)
        if (x.check(st)(seb)) {
          CodeGenerator.currST = st
          val code = generateString(generate(x))
          val pw = new PrintWriter("temp.s")
          pw.write(code)
          pw.close()
          s"arm-linux-gnueabi-gcc -o temp -mcpu=arm1176jzf-s -mtune=arm1176jzf-s temp.s".!
          s"qemu-arm -L /usr/arm-linux-gnueabi/ temp".!!
        } else {
          "ERROR"
        }
      case Failure(_) =>
        // Should throw error if parser failed
        "ERROR"
    }
  }
}
