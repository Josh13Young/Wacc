package wacc.backend

object Operand {

  trait Operand

  case class Immediate(value: Int) extends Operand {
    override def toString: String = s"#$value"
  }

  sealed trait Register extends Operand

  case class Reg(n: Int) extends Register {
    override def toString: String = s"r$n"
  }

  case class StackPointer() extends Register {
    override def toString: String = "sp"
  }

  case class FramePointer() extends Register {
    override def toString: String = "fp"
  }

  case class LinkRegister() extends Register {
    override def toString: String = "lr"
  }

  case class ProgramCounter() extends Register {
    override def toString: String = "pc"
  }

  case class RegOffset(reg: Register, offset: Immediate) extends Operand {
    override def toString: String = s"[$reg, $offset]"
  }

  case class LabelJump(label: String) extends Operand {
    override def toString: String = s"=$label"
  }

  case class ImmediateJump(imm: Immediate) extends Operand {
    override def toString: String = s"=$imm"
  }

  // Operand2 see manual page 1-43 table 1-13
  case class Operand2(reg: Reg, command: String, shift: Immediate) extends Operand {
    override def toString: String = s"$reg, $command $shift"
  }
}
