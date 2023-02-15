package wacc.backend

import wacc.backend.Instruction._

object Translator {

  def translate(instr: Instruction): String = {
    instr match {
      case Label(name) =>
        s"$name:\n"
      case Push(regList) =>
        s"\tpush {${regList.map(reg => reg.toString).mkString(", ")}}\n"
      case Pop(regList) =>
        s"\tpop {${regList.map(reg => reg.toString).mkString(", ")}}\n"
      case Mov(dest, src) =>
        s"\tmov ${dest.toString}, ${src.toString}\n"
      case BranchLink(label) =>
        s"\tbl $label\n"
      case BranchLinkWithCond(cond, label) =>
        s"\tbl$cond $label\n"
      case Directive(name) =>
        s".$name\n"
      case Load(dest, src) =>
        s"\tldr ${dest.toString}, ${src.toString}\n"
      case Compare(reg, operand) =>
        s"\tcmp ${reg.toString}, ${operand.toString}\n"
      case Branch(cond, label) =>
        s"\tb$cond ${label.name}\n"
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
      case _ => ""
    }
  }
}
