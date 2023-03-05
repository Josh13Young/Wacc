package wacc.backend

object Instruction {

  sealed trait Instruction

  case class Push(regList: List[Register]) extends Instruction

  case class Pop(regList: List[Register]) extends Instruction

  case class Move(dest: Register, operand: Operand) extends Instruction

  case class MoveCond(cond: Condition, dest: Register, operand: Operand) extends Instruction

  case class Label(name: String) extends Instruction

  case class BranchLink(label: String) extends Instruction

  case class BranchLinkWithCond(cond: Condition, label: String) extends Instruction

  case class Directive(name: String) extends Instruction

  case class Load(dest: Register, operand: Operand) extends Instruction

  case class LoadRegSignedByte(dest: Register, operand: Operand) extends Instruction

  case class Compare(reg: Register, operand: Operand) extends Instruction

  case class Branch(cond: Condition, operand: Label) extends Instruction

  case class MulInstr(destLo: Register, destHi: Register, register1: Register, register2: Register) extends Instruction

  case class AddInstr(dest: Register, operand1: Operand, operand2: Operand) extends Instruction

  case class SubInstr(dest: Register, operand1: Operand, operand2: Operand) extends Instruction

  case class AndInstr(dest: Register, operand1: Operand, operand2: Operand) extends Instruction

  case class OrInstr(dest: Register, operand1: Operand, operand2: Operand) extends Instruction

  case class Store(dest: Register, operand: Operand) extends Instruction

  case class StoreRegByte(dest: Register, operand: Operand) extends Instruction

  case class Xor(dest: Register, operand1: Operand, operand2: Operand) extends Instruction

  case class RevSub(dest: Register, operand1: Operand, operand2: Operand) extends Instruction

  sealed trait Condition

  case object GreaterThan extends Condition

  case object GreaterEqual extends Condition

  case object LessThan extends Condition

  case object LessEqual extends Condition

  case object Equal extends Condition

  case object NotEqual extends Condition

  case object Overflow extends Condition

  case object Nothing extends Condition

  sealed trait Operand

  case class Immediate(value: Int) extends Operand

  case class RegOffset(reg: Register, offset: Immediate) extends Operand

  case class RegOffsetWriteBack(reg: Register, offset: Immediate) extends Operand

  case class RegOffsetOperand2(reg: Register, offset: Operand2) extends Operand

  case class RegOffsetReg(reg1: Register, reg2: Register) extends Operand

  case class LabelJump(label: String) extends Operand

  case class ImmediateJump(imm: Immediate) extends Operand

  // Operand2 see manual page 1-43 table 1-13
  case class Operand2(reg: Reg, command: String, shift: Immediate) extends Operand

  sealed trait Register extends Operand

  case class Reg(n: Int) extends Register

  case class StackPointer() extends Register

  case class FramePointer() extends Register

  case class LinkRegister() extends Register

  case class ProgramCounter() extends Register
}
