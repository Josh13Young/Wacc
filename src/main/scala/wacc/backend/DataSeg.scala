package wacc.backend

import wacc.backend.Const._
import wacc.backend.Instruction._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// to generate code in data segment (between .data and .text)
object DataSeg {

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
      Store(Reg(0), RegOffsetWriteBack(StackPointer(), Immediate(-WORD))),
      Move(Reg(1), StackPointer()),
      Load(Reg(0), LabelJump(addStrFun("%d"))),
      BranchLink("scanf"),
      Load(Reg(0), RegOffset(StackPointer(), Immediate(0))),
      AddInstr(StackPointer(), StackPointer(), Immediate(WORD)),
      Pop(List(ProgramCounter()))
    )
  }

  def printString(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_str"),
      Push(List(LinkRegister())),
      Move(Reg(2), Reg(0)),
      Load(Reg(1), RegOffset(Reg(0), Immediate(-WORD))),
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
      StoreRegByte(Reg(0), RegOffsetWriteBack(StackPointer(), Immediate(-BYTE))),
      Move(Reg(1), StackPointer()),
      Load(Reg(0), LabelJump(addStrFun(" %c"))),
      BranchLink("scanf"),
      LoadRegSignedByte(Reg(0), RegOffset(StackPointer(), Immediate(0))),
      AddInstr(StackPointer(), StackPointer(), Immediate(BYTE)),
      Pop(List(ProgramCounter()))
    )
  }

  def printBool(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("print_bool"),
      Push(List(LinkRegister())),
      Compare(Reg(0), Immediate(FALSE)),
      Branch(NotEqual, Label("print_false")),
      Load(Reg(2), LabelJump(addStrFun("false"))),
      Branch(Nothing, Label("print_bool_cont")),
      Label("print_false"),
      Load(Reg(2), LabelJump(addStrFun("true"))),
      Label("print_bool_cont"),
      Load(Reg(1), RegOffset(Reg(2), Immediate(-WORD))),
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

  def freePair(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("free_pair"),
      Push(List(LinkRegister())),
      Move(Reg(8), Reg(0)),
      Compare(Reg(8), Immediate(FALSE)),
      BranchLinkWithCond(Equal, "null_error"),
      Load(Reg(0), RegOffset(Reg(8), Immediate(0))),
      BranchLink("free"),
      Load(Reg(0), RegOffset(Reg(8), Immediate(WORD))),
      Move(Reg(0), Reg(8)),
      BranchLink("free"),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayLoad(): ListBuffer[Instruction] = {
    ListBuffer(Label("array_load")) ++ arrayCheckBoundHelper() ++ ListBuffer(
      Load(Reg(3), RegOffsetOperand2(Reg(3), Operand2(Reg(10), "lsl", Immediate(2)))),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayLoadByte(): ListBuffer[Instruction] = {
    ListBuffer(Label("array_load_b")) ++ arrayCheckBoundHelper() ++ ListBuffer(
      LoadRegSignedByte(Reg(3), RegOffsetReg(Reg(3), Reg(10))),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayStore(): ListBuffer[Instruction] = {
    ListBuffer(Label("array_store")) ++ arrayCheckBoundHelper() ++ ListBuffer(
      Store(Reg(8), RegOffsetOperand2(Reg(3), Operand2(Reg(10), "lsl", Immediate(2)))),
      Pop(List(ProgramCounter()))
    )
  }

  def arrayStoreByte(): ListBuffer[Instruction] = {
    ListBuffer(Label("array_store_b")) ++ arrayCheckBoundHelper() ++ ListBuffer(
      StoreRegByte(Reg(8), RegOffsetReg(Reg(3), Reg(10))),
      Pop(List(ProgramCounter()))
    )
  }

  private def arrayCheckBoundHelper(): ListBuffer[Instruction] = {
    ListBuffer(
      Push(List(LinkRegister())),
      Compare(Reg(10), Immediate(FALSE)),
      MoveCond(LessThan, Reg(1), Reg(10)),
      BranchLinkWithCond(LessThan, "bounds_error"),
      Load(LinkRegister(), RegOffset(Reg(3), Immediate(-WORD))),
      Compare(Reg(10), LinkRegister()),
      MoveCond(GreaterEqual, Reg(1), Reg(10)),
      BranchLinkWithCond(GreaterEqual, "bounds_error")
    )
  }

  def overflowError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("overflow_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: integer overflow"))),
      BranchLink("print_str"),
      Move(Reg(0), Immediate(RUNTIME_ERROR)),
      BranchLink("exit")
    )
  }

  def divideByZeroError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("divide_by_zero_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: divide by zero"))),
      BranchLink("print_str"),
      Move(Reg(0), Immediate(RUNTIME_ERROR)),
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
      Move(Reg(0), Immediate(RUNTIME_ERROR)),
      BranchLink("exit")
    )
  }

  def nullError(): ListBuffer[Instruction] = {
    ListBuffer(
      Label("null_error"),
      Load(Reg(0), LabelJump(addStrFun("fatal error: null pointer"))),
      BranchLink("print_str"),
      Move(Reg(0), Immediate(RUNTIME_ERROR)),
      BranchLink("exit")
    )
  }

  // add a string function
  case class StrFuncGen(content: String, ind: String, size: Int) {
    val label: String = s"str_$ind"
    val instructions: ListBuffer[Instruction] = ListBuffer(
      Directive(s"word $size"),
      Label(label),
      Directive(s"asciz \"$content\"")
    )
  }

  // a map of content -> instruction for storing strings
  private val PrintInstrMap: mutable.LinkedHashMap[String, StrFuncGen] = mutable.LinkedHashMap()
  // a counter for the number of string functions (str_0, str_1, ...)
  private var funcTypeCount: Int = 0

  def addStrFun(content: String): String = {
    // return the label if found (label such as str_0, str_1, ...)
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
