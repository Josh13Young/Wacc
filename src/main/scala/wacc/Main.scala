package wacc

import parsley.{Failure, Success}
import wacc.ast.Func
import wacc.backend.CodeGenerator
import wacc.backend.CodeGenerator.{generate, generateString}
import wacc.backend.Peephole.optimise
import wacc.error.WaccErrorBuilder
import wacc.frontend.{SymbolTable, parser}

import java.io.PrintWriter

object Main {
  def main(args: Array[String]): Unit = {
    val stlib = scala.io.Source.fromFile("standard_library/stlib")
    val stlibInputList = try stlib.getLines().toList finally stlib.close()

    val source = scala.io.Source.fromFile(args.head)
    val inputList = try source.getLines().toList finally source.close()

    val input = inputList.mkString("\n")
    var functions = List.empty[Func]

    implicit val eb: WaccErrorBuilder = new WaccErrorBuilder
    parser.funcParser.parse(stlibInputList.mkString("\n")) match {
      case Success(x) => functions = x
      case _ => // not reached
    }
    parser.parser.parse(input) match {
      case Success(x) =>
        val seb = new wacc.error.WaccSemanticErrorBuilder.SemanticError
        seb.program = inputList
        val st = new SymbolTable(None)
        x.functions ++= functions
        if (x.check(st)(seb)) {
          println("Program is semantically correct")
          CodeGenerator.currST = st
          val code = generateString(optimise(generate(x).toList))
          val fileName = args.head.split("/").last.split("\\.").head
          val pw = new PrintWriter(fileName + ".s")
          pw.write(code)
          pw.close()
        } else {
          seb.printAll()
          if (seb.isSemantic) {
            sys.exit(200)
          } else {
            sys.exit(100)
          }
        }
      case Failure(msg) =>
        println(msg)
        sys.exit(100)
    }
  }
}
