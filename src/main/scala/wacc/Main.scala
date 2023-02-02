package wacc

import parsley.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
//    val source = scala.io.Source.fromFile(args.head)
//    val input = try source.getLines().toList.mkString("\n") finally source.close()
    parser.parser.parse(args.head) match {
      case Success(x) =>
        println(s"${args.head} = $x")
        if (x.check(new SymbolTable(None))) {
          println("Program is semantically correct")
        } else {
          println("Program is semantically incorrect")
          // sys.exit(200)
        }
      case Failure(msg) =>
        println(msg)
//        sys.exit(100)
    }
  }
}
