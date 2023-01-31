package wacc

import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Failure, Success}

import java.io.File

class ExampleTest extends AnyFlatSpec {
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  "test basic exit" should "succeed" in {
    val input = "begin return 3 end"
    val files = getListOfFiles("wacc_examples/valid/basic/exit")
    val result: List[String] = List()
    // you can put the success result in the result list above and compare it with the expected result
    for (file <- files) {
      println(file)
      val source = scala.io.Source.fromFile(file)
      val input = try source.getLines().toList.mkString("\n") finally source.close()
      parser.parser.parse(input) match {
        case Success(x) => println(s"$input = $x")
        case Failure(msg) => println(msg)
      }
    }
    val answer = "Program(List(),List(Return(IntLiter(3))))"
  }

}
