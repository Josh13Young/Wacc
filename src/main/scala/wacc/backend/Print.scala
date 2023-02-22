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
      Move(Reg(1), Reg(0)),
      Load(Reg(0), LabelJump(addStrFun("%d"))),
      BranchLink("printf"),
      Move(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  def readInt(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("read_int"),
      Push(List(LinkRegister())),
      Store(Reg(0), RegOffsetWriteBack(StackPointer(), Immediate(-4))),
      Move(Reg(1), StackPointer()),
      Load(Reg(0), LabelJump(addStrFun("%d"))),
      BranchLink("scanf"),
      Load(Reg(0), RegOffset(StackPointer(), Immediate(0))),
      AddInstr(StackPointer(), StackPointer(), Immediate(4)),
      Pop(List(ProgramCounter()))
    )
  }

  def printString(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_str"),
      Push(List(LinkRegister())),
      Move(Reg(2), Reg(0)),
      Load(Reg(1), RegOffset(Reg(0), Immediate(-4))),
      Load(Reg(0), LabelJump(addStrFun("%.*s"))),
      BranchLink("printf"),
      Move(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  def printChar(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_char"),
      Push(List(LinkRegister())),
      Move(Reg(1), Reg(0)),
      Load(Reg(0), LabelJump(addStrFun("%c"))),
      BranchLink("printf"),
      Move(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  def readChar(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("read_char"),
      Push(List(LinkRegister())),
      StoreRegByte(Reg(0), RegOffsetWriteBack(StackPointer(), Immediate(-1))),
      Move(Reg(1), StackPointer()),
      Load(Reg(0), LabelJump(addStrFun(" %c"))),
      BranchLink("scanf"),
      LoadRegSignedByte(Reg(0), RegOffset(StackPointer(), Immediate(0))),
      AddInstr(StackPointer(), StackPointer(), Immediate(1)),
      Pop(List(ProgramCounter()))
    )
  }

  def printAddr(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_addr"),
      Push(List(LinkRegister())),
      Move(Reg(1), Reg(0)),
      Load(Reg(0), LabelJump(addStrFun("%p"))),
      BranchLink("printf"),
      Move(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayLoad(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("array_load"),
      Push(List(LinkRegister())),
      Compare(Reg(10), Immediate(0)),
      MoveCond("lt", Reg(1), Reg(10)),
      BranchLinkWithCond("lt", "bounds_error"),
      Load(LinkRegister(), RegOffset(Reg(3), Immediate(-4))),
      Compare(Reg(10), LinkRegister()),
      MoveCond("ge", Reg(1), Reg(10)),
      BranchLinkWithCond("ge", "bounds_error"),
      Load(Reg(3), RegOffsetOperand2(Reg(3), Operand2(Reg(10), "lsl", Immediate(2)))),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayLoadByte(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("array_load_b"),
      Push(List(LinkRegister())),
      Compare(Reg(10), Immediate(0)),
      MoveCond("lt", Reg(1), Reg(10)),
      BranchLinkWithCond("lt", "bounds_error"),
      Load(LinkRegister(), RegOffset(Reg(3), Immediate(-4))),
      Compare(Reg(10), LinkRegister()),
      MoveCond("ge", Reg(1), Reg(10)),
      BranchLinkWithCond("ge", "bounds_error"),
      LoadRegSignedByte(Reg(3), RegOffsetReg(Reg(3), Reg(10))),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayStore(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("array_store"),
      Push(List(LinkRegister())),
      Compare(Reg(10), Immediate(0)),
      MoveCond("lt", Reg(1), Reg(10)),
      BranchLinkWithCond("lt", "bounds_error"),
      Load(LinkRegister(), RegOffset(Reg(3), Immediate(-4))),
      Compare(Reg(10), LinkRegister()),
      MoveCond("ge", Reg(1), Reg(10)),
      BranchLinkWithCond("ge", "bounds_error"),
      Store(Reg(8), RegOffsetOperand2(Reg(3), Operand2(Reg(10), "lsl", Immediate(2)))),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayStoreByte(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("array_store_b"),
      Push(List(LinkRegister())),
      Compare(Reg(10), Immediate(0)),
      MoveCond("lt", Reg(1), Reg(10)),
      BranchLinkWithCond("lt", "bounds_error"),
      Load(LinkRegister(), RegOffset(Reg(3), Immediate(-4))),
      Compare(Reg(10), LinkRegister()),
      MoveCond("ge", Reg(1), Reg(10)),
      BranchLinkWithCond("ge", "bounds_error"),
      StoreRegByte(Reg(8), RegOffsetReg(Reg(3), Reg(10))),
      Pop(List(ProgramCounter()))
    )
  }

  def overflowError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("overflow_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: integer overflow"))),
      BranchLink("print_str"),
      Move(Reg(0), Immediate(255)),
      BranchLink("exit")
    )
  }

  def divideByZeroError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("divide_by_zero_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: divide by zero"))),
      BranchLink("print_str"),
      Move(Reg(0), Immediate(255)),
      BranchLink("exit")
    )
  }

  def boundsError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("bounds_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: array index out of bounds"))),
      BranchLink("printf"),
      Move(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Move(Reg(0), Immediate(255)),
      BranchLink("exit")
    )
  }

  def nullError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("null_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: null pointer"))),
      BranchLink("print_str"),
      Move(Reg(0), Immediate(255)),
      BranchLink("exit")
    )
  }

  def printBool(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_bool"),
      Push(List(LinkRegister())),
      Compare(Reg(0), Immediate(0)), // 0 is false
      Branch("ne", Label("print_false")),
      Load(Reg(2), LabelJump(addStrFun("false"))),
      Branch("", Label("print_bool_cont")),
      Label("print_false"),
      Load(Reg(2), LabelJump(addStrFun("true"))),
      Label("print_bool_cont"),
      Load(Reg(1), RegOffset(Reg(2), Immediate(-4))),
      Load(Reg(0), LabelJump(addStrFun("%.*s"))),
      BranchLink("printf"),
      Move(Reg(0), Immediate(0)),
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
      Move(Reg(0), Immediate(0)),
      BranchLink("fflush"),
      Pop(List(ProgramCounter()))
    )
  }

  case class StrFuncGen(content: String, ind: String, size: Int) {
    val label: String = s"str_$ind"
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
