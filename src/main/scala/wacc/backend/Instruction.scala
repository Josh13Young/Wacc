package wacc.backend

import wacc.backend.Operand._

object Instruction {

  sealed trait Instruction

  case class Push(regList: List[Register]) extends Instruction

  case class Pop(regList: List[Register]) extends Instruction

  case class Mov(dest: Register, operand: Operand) extends Instruction

  case class Label(name: String) extends Instruction // probably not belong here

  case class BranchLink(label: String) extends Instruction

  case class Directive(name: String) extends Instruction

  case class Load(dest: Register, operand: Operand) extends Instruction

  case class Compare(reg: Register, operand: Operand) extends Instruction

  case class Branch(cond: String, operand: Label) extends Instruction // todo change cond into enum

}
