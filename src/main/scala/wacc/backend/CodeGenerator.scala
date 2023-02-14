package wacc.backend

import wacc.ast._
import wacc.backend.Instruction._
import wacc.backend.Operand._
import wacc.backend.Print.{addStrFun, getPrintInstr, printChar, printInt, printLn, printString}
import wacc.backend.Translator.translate

import scala.collection.mutable.ListBuffer

object CodeGenerator {

  def generateString(listBuffer: ListBuffer[Instruction]): String = {
    listBuffer.map(instr => translate(instr)).mkString ++ "\n"
  }

  private var nonMainFunc: Map[String, ListBuffer[Instruction]] = Map()

  def generate(ast: ASTNode): ListBuffer[Instruction] = {
    val mainStart = ListBuffer(
      Directive("global main"),
      Label("main"),
      Push(List(FramePointer(), LinkRegister())),
      Mov(FramePointer(), StackPointer()))

    val mainEnd = ListBuffer(
      Mov(Reg(0), Immediate(0)),
      Mov(StackPointer(), FramePointer()),
      Pop(List(FramePointer(), ProgramCounter())))

    ast match {
      case Program(functions, stat) =>
        val statGen = stat.map(s => generate(s))
        mainStart ++ statGen.flatten ++ mainEnd ++ nonMainFunc.values.flatten ++ ListBuffer(Directive("data")) ++ getPrintInstr ++
          ListBuffer(Directive("text"))
      case Exit(expr) =>
        val exitGen = exprGen(expr, 8)
        val exit = ListBuffer(Mov(Reg(0), Reg(8)), BranchLink("exit"))
        exitGen ++ exit
      case Print(expr) =>
        val printGen = exprGen(expr, 8)
        expr match {
          case IntLiter(_) =>
            val print = ListBuffer(BranchLink("print_int"))
            nonMainFunc += ("print_int" -> printInt())
            printGen ++ print
          case StrLiter(_) =>
            val print = ListBuffer(BranchLink("print_str"))
            nonMainFunc += ("print_str" -> printString())
            printGen ++ print
          case CharLiter(_) =>
            val print = ListBuffer(BranchLink("print_char"))
            nonMainFunc += ("print_char" -> printChar())
            printGen ++ print
          case _ => ListBuffer()
        }
      case Println(expr) =>
        val normalPrint = generate(Print(expr)(expr.pos))
        nonMainFunc += ("print_ln" -> printLn())
        normalPrint ++ ListBuffer(BranchLink("print_ln"))
      case _ => ListBuffer()
    }
  }

  private def exprGen(expr: Expr, reg: Int): ListBuffer[Instruction] = {
    expr match {
      case IntLiter(value) =>
        ListBuffer(Mov(Reg(reg), Immediate(value)), Mov(Reg(0), Reg(reg)))
      case StrLiter(value) =>
        val label = addStrFun(value)
        ListBuffer(Load(Reg(reg), LabelJump(label)), Mov(Reg(0), Reg(reg)))
      case CharLiter(value) =>
        ListBuffer(Mov(Reg(reg), Immediate(value.toInt)), Mov(Reg(0), Reg(reg)))
      case _ => ListBuffer()
    }
  }
}
