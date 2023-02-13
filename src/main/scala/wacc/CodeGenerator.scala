package wacc

import wacc.ast._

object CodeGenerator {

  def generate(ast: ASTNode): String = {
    val start = ".data\n.text\n.global main\n"
    val mainStart = "main:\n\tpush {fp, lr}\n\tmov fp, sp\n"
    val mainEnd = "\tmov r0, #0\n\tpop {fp, pc}\n"
    val sb = new StringBuilder
    ast match {
      case Program(functions, stat) =>
        sb.append(start)
        sb.append(mainStart)
        stat.map(s => sb.append(generate(s)))
        sb.append(mainEnd)
      case Exit(expr) =>
        sb.append(exprGen(expr))
      case _ =>
    }
    sb.toString()
  }

  private def exprGen(expr: Expr): String = {
    expr match {
      case IntLiter(value) =>
        s"\tmov r8, #$value\n\tmov r0, r8\n\tbl exit\n"
      case _ => ""
    }
  }
}
