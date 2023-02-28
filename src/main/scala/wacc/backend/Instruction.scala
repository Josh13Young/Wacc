package wacc.backend

import wacc.backend.Operand._

object Instruction {

  sealed trait Instruction

  case class Push(regList: List[Register]) extends Instruction

  case class Pop(regList: List[Register]) extends Instruction

  case class Move(dest: Register, operand: Operand) extends Instruction

  case class MoveCond(cond: Condition, dest: Register, operand: Operand) extends Instruction

  case class Label(name: String) extends Instruction // probably not belong here

  case class BranchLink(label: String) extends Instruction

  case class BranchLinkWithCond(cond: String, label: String) extends Instruction

  case class Directive(name: String) extends Instruction

  case class Load(dest: Register, operand: Operand) extends Instruction

  case class LoadRegSignedByte(dest: Register, operand: Operand) extends Instruction

  case class Compare(reg: Register, operand: Operand) extends Instruction

  case class Branch(cond: String, operand: Label) extends Instruction // todo change cond into enum

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

  case object Greater extends Condition

  case object GreaterEqual extends Condition

  case object Less extends Condition

  case object LessEqual extends Condition

  case object Equal extends Condition

  case object NotEqual extends Condition
}
