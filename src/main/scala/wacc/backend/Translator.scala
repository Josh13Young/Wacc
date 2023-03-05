package wacc.backend

import wacc.backend.Instruction._

object Translator {

  // adds a tab in front of each instruction (not label and directive)
  def translate(instr: Instruction): String = instr match {
    case Label(name) =>
      s"$name:\n"
    case Push(regList) =>
      s"\tpush {${regList.map(translateOperand).mkString(", ")}}\n"
    case Pop(regList) =>
      s"\tpop {${regList.map(translateOperand).mkString(", ")}}\n"
    case Move(dest, src) =>
      s"\tmov ${translateOperand(dest)}, ${translateOperand(src)}\n"
    case MoveCond(cond, dest, operand) =>
      s"\tmov${translateCond(cond)} ${translateOperand(dest)}, ${translateOperand(operand)}\n"
    case BranchLink(label) =>
      s"\tbl $label\n"
    case BranchLinkWithCond(cond, label) =>
      s"\tbl${translateCond(cond)} $label\n"
    case Directive(name) =>
      s".$name\n"
    case Load(dest, src) =>
      s"\tldr ${translateOperand(dest)}, ${translateOperand(src)}\n"
    case LoadRegSignedByte(dest, src) =>
      s"\tldrsb ${translateOperand(dest)}, ${translateOperand(src)}\n"
    case Compare(reg, operand) =>
      s"\tcmp ${translateOperand(reg)}, ${translateOperand(operand)}\n"
    case Branch(cond, label) =>
      s"\tb${translateCond(cond)} ${label.name}\n"
    case MulInstr(destLo, destHi, reg1, reg2) =>
      s"\tsmull ${translateOperand(destLo)}, ${translateOperand(destHi)}, ${translateOperand(reg1)}, ${translateOperand(reg2)}\n"
    case AddInstr(dest, operand1, operand2) =>
      s"\tadds ${translateOperand(dest)}, ${translateOperand(operand1)}, ${translateOperand(operand2)}\n"
    case SubInstr(dest, operand1, operand2) =>
      s"\tsubs ${translateOperand(dest)}, ${translateOperand(operand1)}, ${translateOperand(operand2)}\n"
    case AndInstr(dest, operand1, operand2) =>
      s"\tand ${translateOperand(dest)}, ${translateOperand(operand1)}, ${translateOperand(operand2)}\n"
    case OrInstr(dest, operand1, operand2) =>
      s"\torr ${translateOperand(dest)}, ${translateOperand(operand1)}, ${translateOperand(operand2)}\n"
    case Store(dest, operand) =>
      s"\tstr ${translateOperand(dest)}, ${translateOperand(operand)}\n"
    case StoreRegByte(dest, operand) =>
      s"\tstrb ${translateOperand(dest)}, ${translateOperand(operand)}\n"
    case Xor(dest, operand1, operand2) =>
      s"\teor ${translateOperand(dest)}, ${translateOperand(operand1)}, ${translateOperand(operand2)}\n"
    case RevSub(dest, operand1, operand2) =>
      s"\trsbs ${translateOperand(dest)}, ${translateOperand(operand1)}, ${translateOperand(operand2)}\n"
    case _ => ""
  }

  private def translateOperand(op: Operand): String = op match {
    case Reg(reg) => s"r$reg"
    case StackPointer() => "sp"
    case LinkRegister() => "lr"
    case ProgramCounter() => "pc"
    case FramePointer() => "fp"
    case Immediate(value) => s"#$value"
    case RegOffset(reg, offset) => s"[${translateOperand(reg)}, ${translateOperand(offset)}]"
    case RegOffsetWriteBack(reg, offset) => s"[${translateOperand(reg)}, ${translateOperand(offset)}]!"
    case RegOffsetOperand2(reg, offset) => s"[${translateOperand(reg)}, ${translateOperand(offset)}]"
    case RegOffsetReg(reg1, reg2) => s"[${translateOperand(reg1)}, ${translateOperand(reg2)}]"
    case LabelJump(label) => s"=$label"
    case ImmediateJump(value) => s"=${translateOperand(value)}"
    case Operand2(reg, cmd, sft) => s"${translateOperand(reg)}, $cmd ${translateOperand(sft)}"
  }

  private def translateCond(cond: Condition): String = cond match {
    case GreaterThan => "gt"
    case GreaterEqual => "ge"
    case LessThan => "lt"
    case LessEqual => "le"
    case Equal => "eq"
    case NotEqual => "ne"
    case Overflow => "vs"
    case Nothing => ""
  }
}
