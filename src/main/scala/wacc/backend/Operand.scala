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
}
