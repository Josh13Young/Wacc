package wacc.backend

import wacc.backend.Instruction._
import wacc.backend.Operand._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Print {

  def printInt(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_int"),
      Push(List(LinkRegister())),
      Mov(Reg(1), Reg(0)),
      Load(Reg(0), LabelJump(addStrFun("%d"))),
      BranchLink("printf"),
      Mov(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  def printString(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_str"),
      Push(List(LinkRegister())),
      Mov(Reg(2), Reg(0)),
      Load(Reg(1), RegOffset(Reg(0), Immediate(-4))),
      Load(Reg(0), LabelJump(addStrFun("%.*s"))),
      BranchLink("printf"),
      Mov(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  def printLn(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_ln"),
      Push(List(LinkRegister())),
      Load(Reg(0), LabelJump(addStrFun(""))),
      BranchLink("puts"),
      Mov(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  case class StrFuncGen(content: String, ind: String, size: Int) {
    val label: String = s"msg_$ind"
    val instructions: ListBuffer[Instruction] = ListBuffer(
      Directive(s"word $size"),
      Label(label),
      Directive(s"asciz \"$content\"")
    )
  }

  private val PrintInstrMap: mutable.LinkedHashMap[String, StrFuncGen] = mutable.LinkedHashMap()
  private var funcTypeCount: Int = 0

  def addStrFun(content: String): String = {
    if (PrintInstrMap.contains(content)) {
      return PrintInstrMap(content).label
    }
    val msg = StrFuncGen(content, funcTypeCount.toString, content.length)
    funcTypeCount += 1
    PrintInstrMap += (content -> msg)
    msg.label
  }

  def getPrintInstr: mutable.Buffer[Instruction] = {
    PrintInstrMap.values.flatMap(_.instructions).toBuffer
  }
}
