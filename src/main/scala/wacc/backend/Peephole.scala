package wacc.backend

import wacc.backend.Instruction._

object Peephole {

  def optimise(instructions: List[Instruction]): List[Instruction] =
    removeBinOpPushPop(removeMovAfterMov(removeMovAfterLdr(removeMovToRegZero(removeZeroStackPointer(removeLdrAfterStr(instructions))))))

  /*
  Remove the following push / pop wrapped around:
  push {r8}
  ldr rX, blah (X != 8) or mov rX, blah (X != 8)
  pop {r8}

  Only effective after running removeMovAfterLdr (so that the content inside push/pop is doesn't use r8
   */
  private def removeBinOpPushPop(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case Push(List(Reg(8))) :: xs =>
        xs.head match {
          case Load(reg, _) =>
            xs.tail match {
              case Pop(List(Reg(8))) :: ys =>
                if (!equalReg(reg, Reg(8))) {
                  xs.head :: removeBinOpPushPop(ys)
                } else {
                  instructions.head :: removeBinOpPushPop(instructions.tail)
                }
              case _ => instructions.head :: removeBinOpPushPop(instructions.tail)
            }
          case Move(reg, _) =>
            xs.tail match {
              case Pop(List(Reg(8))) :: ys =>
                if (!equalReg(reg, Reg(8))) {
                  xs.head :: removeBinOpPushPop(ys)
                } else {
                  instructions.head :: removeBinOpPushPop(instructions.tail)
                }
              case _ => instructions.head :: removeBinOpPushPop(instructions.tail)
            }
          case _ => instructions.head :: removeBinOpPushPop(instructions.tail)
        }
      case _ => instructions.head :: removeBinOpPushPop(instructions.tail)
    }
  }

  /*
  Change the following pattern:
  mov rX, blah
  mov rY, rX

  into:
  mov rY, blah
   */

  private def removeMovAfterMov(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case Move(dest, op) :: xs =>
        xs.head match {
          case Move(dest2, op2) =>
            op2 match {
              case reg: Register =>
                if (equalReg(dest, reg)) {
                  Move(dest2, op) :: removeMovAfterMov(xs.tail)
                } else {
                  instructions.head :: removeMovAfterMov(instructions.tail)
                }
              case _ => instructions.head :: removeMovAfterMov(instructions.tail)
            }
          case _ => instructions.head :: removeMovAfterMov(instructions.tail)
        }
      case _ => instructions.head :: removeMovAfterMov(instructions.tail)
    }
  }

  /*
  Dealt with the following pattern:

  ldr rX, blah
  mov rY, rX

  change into ldr rY, blah
   */

  private def removeMovAfterLdr(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case Load(dest, op) :: xs =>
        xs.head match {
          case Move(dest2, op2) =>
            op2 match {
              case reg: Register =>
                if (equalReg(dest, reg)) {
                  Load(dest2, op) :: removeMovAfterLdr(xs.tail)
                } else {
                  instructions.head :: removeMovAfterLdr(instructions.tail)
                }
              case _ => instructions.head :: removeMovAfterLdr(instructions.tail)
            }
          case _ => instructions.head :: removeMovAfterLdr(instructions.tail)
        }
      case _ => instructions.head :: removeMovAfterLdr(instructions.tail)
    }
  }

  /*
  Remove the following pattern:
  mov r0, r8

  r0 is only used before calling a branched function, so preserve before branched call
   */

  private def removeMovToRegZero(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case Move(dest, src) :: xs =>
        src match {
          case Reg(8) =>
            xs.head match {
              case BranchLink(_) | BranchLinkWithCond(_, _) =>
                instructions.head :: removeMovToRegZero(instructions.tail)
              case _ =>
                if (equalReg(dest, Reg(0))) {
                  removeMovToRegZero(xs)
                } else {
                  instructions.head :: removeMovToRegZero(instructions.tail)
                }
            }
          case _ => instructions.head :: removeMovToRegZero(instructions.tail)
        }
      case _ => instructions.head :: removeMovToRegZero(instructions.tail)
    }
  }

  /*
  Remove add/sub to stack pointer with 0 offset
  add/sub sp, sp, #0
   */

  private def removeZeroStackPointer(instructions: List[Instruction]): List[Instruction] =
    instructions.filterNot(x => x == AddInstr(StackPointer(), StackPointer(), Immediate(0)) || x == SubInstr(StackPointer(), StackPointer(), Immediate(0)))

  /*
  Remove the following ldr instruction:
  str rX, [rY, #imm]
  blah blah blah (doesn't modify rX)
  ldr rX, [rY, #imm]
   */
  private def removeLdrAfterStr(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case Store(dest, RegOffset(src, Immediate(imm))) :: xs =>
        instructions.head :: removeLdrAfterStr(locateLoadHelper(xs, dest, src, imm))
      case _ => instructions.head :: removeLdrAfterStr(instructions.tail)

    }
  }

  private def locateLoadHelper(xs: List[Instruction], dest: Register, src: Register, imm: Int): List[Instruction] = {
    xs match {
      case Nil => Nil
      case Store(dest2, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case StoreRegByte(dest2, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case Move(dest2, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case MoveCond(_, dest2, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case LoadRegSignedByte(dest2, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case AddInstr(dest2, _, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case SubInstr(dest2, _, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case AndInstr(dest2, _, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case OrInstr(dest2, _, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case Xor(dest2, _, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case RevSub(dest2, _, _) :: _ =>
        if (equalReg(dest, dest2)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case Pop(regs) :: _ =>
        if (regs.contains(dest)) {
          xs
        } else {
          xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
        }
      case BranchLink(_) :: _ =>
        xs
      case BranchLinkWithCond(_, _) :: _ =>
        xs
      case Label(_) :: _ =>
        xs
      case Directive(_) :: _ =>
        xs
      case Load(dest2, RegOffset(src2, Immediate(imm2))) :: ys =>
        if (equalReg(dest, dest2) && equalReg(src, src2) && imm == imm2) {
          ys
        } else {
          xs
        }
      case _ => xs.head :: locateLoadHelper(xs.tail, dest, src, imm)
    }
  }

  //equalReg checks if two registers are equal

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
