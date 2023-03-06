package wacc.backend

import wacc.backend.Instruction.{FramePointer, Immediate, Instruction, LinkRegister, Load, ProgramCounter, Reg, RegOffset, Register, StackPointer, Store}

object Peephole {

  def removeStrAfterLdr(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case Store(dest, RegOffset(src, Immediate(imm))) :: xs =>
        instructions.head :: removeStrAfterLdr(locateLoadHelper(xs, dest, src, imm))
      case _ => instructions.head :: removeStrAfterLdr(instructions.tail)

    }
  }

  private def locateLoadHelper(xs: List[Instruction], dest: Register, src: Register, imm: Int): List[Instruction] = {
    xs match {
      case Nil => Nil
      case Load(dest2, RegOffset(src2, Immediate(imm2))) :: ys =>
        if (equalReg(dest, dest2) && equalReg(src, src2) && imm == imm2) {
          removeStrAfterLdr(ys)
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case _ => xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
    }
  }

  private def equalReg(reg1: Register, reg2: Register): Boolean = {
    (reg1, reg2) match {
      case (Reg(n1), Reg(n2)) => n1 == n2
      case (FramePointer(), FramePointer()) => true
      case (StackPointer(), StackPointer()) => true
      case (LinkRegister(), LinkRegister()) => true
      case (ProgramCounter(), ProgramCounter()) => true
      case _ => false
    }
  }
}
