package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.matchPattern
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.frontend.{SymbolTable, parser}

import java.io.{ByteArrayOutputStream, File, PrintWriter}
import scala.sys.process._
import java.io.ByteArrayInputStream

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

  def getExpectedOutput(inputList: List[String]): String = {
    inputList.dropWhile(!_.startsWith("# Output:")).drop(1).takeWhile(_.nonEmpty).map(_.drop(2)).mkString("\n")
  }

  def getExpectedInput(inputList: List[String]): Option[String] = {
    if (inputList.exists(_.startsWith("# Input")))
      Some(inputList.dropWhile(!_.startsWith("# Input:")).head.replace("# Input: ", ""))
    else
      None
  }

  def getExpectedExitValue(inputList: List[String]): Option[Int] = {
    inputList.indexOf("# Exit:") match {
      case -1 => None
      case x => Some(inputList(x + 1).drop(2).toInt)
    }
  }

  def assemblyRunner(inputList: List[String]): (Int, String) = {
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
          val outputStream = new ByteArrayOutputStream
          val expectedInput = 
            getExpectedInput(inputList) match {
              case Some(x) => x
              case None => ""
            }
          val inputStream = new ByteArrayInputStream(expectedInput.getBytes())   
          // see https://stackoverflow.com/questions/216894/get-an-outputstream-into-a-string
          val exitCode = (s"qemu-arm -L /usr/arm-linux-gnueabi/ temp" #< inputStream #> outputStream).!
          val output = outputStream.toString
          val filterOutput = filterAddress(output)
          (exitCode, filterOutput)
        } else {
          (200, "ERROR")
        }
      case Failure(_) =>
        // Should throw error if parser failed
        (100, "ERROR")
    }
  }

  def assemblyRunFolder(dir: String): Unit = {
    val files = getListOfFiles(dir)
    for (file <- files) {
      val inputList = getInputList(file)
      val output = assemblyRunner(inputList)
      val expectedExitValue =
        getExpectedExitValue(inputList) match {
          case Some(x) => x
          case None => 0
        }
      if(expectedExitValue == 255) {
        output._1 shouldBe expectedExitValue
      } else {
        output shouldBe (expectedExitValue, getExpectedOutput(inputList))
      }
    }
  }

  def filterAddress(addr: String): String =
    addr.replaceAll("0x[0-9a-z]{5} ", "#addrs# ")
      .replaceAll("0x[0-9a-z]{5}\n", "#addrs#\n")
}
