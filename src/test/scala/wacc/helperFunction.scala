package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.matchPattern
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import parsley.{Failure, Success}
import wacc.frontend.parser

import java.io.File

object helperFunction extends AnyFlatSpec{

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
}
