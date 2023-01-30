package wacc

import parsley.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile(args.head)
    val input = try source.getLines().toList.mkString("\n") finally source.close()
    parser.parser.parse(input) match {
      case Success(x) => println(s"$input = $x")
      case Failure(msg) => println(msg)
    }
  }
}
