package wacc

import wacc.ast.{ASTNode, Program, Skip}

object CodeGenerator {

  def generate(ast: ASTNode): String = {
    val start = ".data\n.text\n.global main\n"
    val mainStart = "main:\n\tpush {fp, lr}\n\tmov fp, sp\n"
    val mainEnd = "\tmov r0, #0\n\tpop {fp, pc}"
    val sb = new StringBuilder
    ast match {
      case Program(functions, stat) =>
        sb.append(start)
        sb.append(mainStart)
        stat.map(s => sb.append(generate(s)))
        sb.append(mainEnd)
      case _ =>
    }
    sb.toString()
  }
}
