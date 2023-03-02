package wacc.backend

import wacc.backend.Instruction._

object Translator {

  // adds a tab in front of each instruction (not label and directive)
  def translate(instr: Instruction): String = {
    instr match {
      case Label(name) =>
        s"$name:\n"
      case Push(regList) =>
        s"\tpush {${regList.map(reg => reg.toString).mkString(", ")}}\n"
      case Pop(regList) =>
        s"\tpop {${regList.map(reg => reg.toString).mkString(", ")}}\n"
      case Move(dest, src) =>
        s"\tmov ${dest.toString}, ${src.toString}\n"
      case MoveCond(cond, dest, operand) =>
        val condStr = translateCond(cond)
        s"\tmov$condStr ${dest.toString}, ${operand.toString}\n"
      case BranchLink(label) =>
        s"\tbl $label\n"
      case BranchLinkWithCond(cond, label) =>
        val condStr = translateCond(cond)
        s"\tbl$condStr $label\n"
      case Directive(name) =>
        s".$name\n"
      case Load(dest, src) =>
        s"\tldr ${dest.toString}, ${src.toString}\n"
      case LoadRegSignedByte(dest, src) =>
        s"\tldrsb ${dest.toString}, ${src.toString}\n"
      case Compare(reg, operand) =>
        s"\tcmp ${reg.toString}, ${operand.toString}\n"
      case Branch(cond, label) =>
        val condStr = translateCond(cond)
        s"\tb$condStr ${label.name}\n"
      case MulInstr(destLo, destHi, register1, register2) =>
        s"\tsmull ${destLo.toString}, ${destHi.toString}, ${register1.toString}, ${register2.toString}\n"
      case AddInstr(dest, operand1, operand2) =>
        s"\tadds ${dest.toString}, ${operand1.toString}, ${operand2.toString}\n"
      case SubInstr(dest, operand1, operand2) =>
        s"\tsubs ${dest.toString}, ${operand1.toString}, ${operand2.toString}\n"
      case AndInstr(dest, operand1, operand2) =>
        s"\tand ${dest.toString}, ${operand1.toString}, ${operand2.toString}\n"
      case OrInstr(dest, operand1, operand2) =>
        s"\torr ${dest.toString}, ${operand1.toString}, ${operand2.toString}\n"
      case Store(dest, operand) =>
        s"\tstr ${dest.toString}, ${operand.toString}\n"
      case StoreRegByte(dest, operand) =>
        s"\tstrb ${dest.toString}, ${operand.toString}\n"
      case Xor(dest, operand1, operand2) =>
        s"\teor ${dest.toString}, ${operand1.toString}, ${operand2.toString}\n"
      case RevSub(dest, operand1, operand2) =>
        s"\trsbs ${dest.toString}, ${operand1.toString}, ${operand2.toString}\n"
      case _ => ""
    }
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
