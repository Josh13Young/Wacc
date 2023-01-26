package wacc

import parsley.{Failure, Success}

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_32!")

        parser.parser.parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
    }
}

