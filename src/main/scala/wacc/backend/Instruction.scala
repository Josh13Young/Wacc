package wacc.backend

import Operand._

object Instruction {

  sealed trait Instruction

  case class Push(regList: List[Register]) extends Instruction

  case class Pop(regList: List[Register]) extends Instruction

  case class Mov(dest: Register, operand: Operand) extends Instruction

  case class Label(name: String) extends Instruction // probably not belong here

}
