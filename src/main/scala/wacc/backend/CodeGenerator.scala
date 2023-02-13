package wacc.backend

import Instruction._
import Operand._
import wacc.ast._
import wacc.backend.Translator.translate

import scala.collection.mutable.ListBuffer

object CodeGenerator {

  def generateString(listBuffer: ListBuffer[Instruction]): String = {
    val start = ".data\n.text\n.global main\n"
    start ++ listBuffer.map(instr => translate(instr)).mkString ++ "\n"
  }

  def generate(ast: ASTNode): ListBuffer[Instruction] = {
    val mainStart = new ListBuffer[Instruction]()
    mainStart += Label("main")
    mainStart += Push(List(FramePointer(), LinkRegister()))
    mainStart += Mov(FramePointer(), StackPointer())

    val mainEnd = new ListBuffer[Instruction]()
    mainEnd += Mov(Reg(0), Immediate(0))
    mainEnd += Pop(List(FramePointer(), LinkRegister()))

    ast match {
      case Program(functions, stat) =>
        val statGen = stat.map(s => generate(s))
        mainStart ++ statGen.flatten ++ mainEnd
      case Exit(expr) =>
        val exitGen = exprGen(expr, 8)
        val exit = Mov(Reg(0), Reg(8))
        exitGen += exit
      case _ => ListBuffer()
    }
  }

  private def exprGen(expr: Expr, reg: Int): ListBuffer[Instruction] = {
    expr match {
      case IntLiter(value) =>
        ListBuffer(Mov(Reg(reg), Immediate(value)))
      case _ => ListBuffer()
    }
  }
}
