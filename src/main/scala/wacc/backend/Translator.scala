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
      case Directive(name) =>
        s".$name\n"
      case Load(dest, src) =>
        s"\tldr ${dest.toString}, ${src.toString}\n"
      case _ => ""
    }
  }
}
