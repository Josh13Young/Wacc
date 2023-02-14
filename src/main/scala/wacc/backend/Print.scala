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

  case class StrFuncGen(s: String, ind: String, size: Int) {
    val _type: String = s
    val label: String = s"msg_$ind"
    val instructions: ListBuffer[Instruction] = ListBuffer(
      Directive(s"word $size"),
      Label(label),
      Directive(s"asciz \"$s\"")
    )
  }

  private val PrintInstrMap: mutable.LinkedHashMap[String, StrFuncGen] = mutable.LinkedHashMap()
  private var funcTypeCount: Int = 0

  private def addStrFun(_type: String): String = {
    if (PrintInstrMap.contains(_type)) {
      return PrintInstrMap(_type).label
    }
    val msg = StrFuncGen(_type, funcTypeCount.toString, _type.length)
    funcTypeCount += 1
    PrintInstrMap += (_type -> msg)
    msg.label
  }

  def getPrintInstr: mutable.Buffer[Instruction] = {
    PrintInstrMap.values.flatMap(_.instructions).toBuffer
  }
}
