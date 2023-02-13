package wacc.backend

import Instruction._

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
    }
  }
}
