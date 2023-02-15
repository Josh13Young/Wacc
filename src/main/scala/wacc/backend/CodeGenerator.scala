package wacc.backend

import wacc.ast._
import wacc.backend.Instruction._
import wacc.backend.Operand._
import wacc.backend.Print._
import wacc.backend.Stack.{addVar, getVar}
import wacc.backend.Translator.translate
import wacc.frontend.STType.{BoolST, CharST, IntST, StringST}
import wacc.frontend.SymbolTable

import scala.collection.mutable.ListBuffer

object CodeGenerator {

  def generateString(listBuffer: ListBuffer[Instruction]): String = {
    listBuffer.map(instr => translate(instr)).mkString ++ "\n"
  }

  private var nonMainFunc: Map[String, ListBuffer[Instruction]] = Map()

  var st: SymbolTable = _

  def generate(ast: ASTNode): ListBuffer[Instruction] = {
    val mainStart = ListBuffer(
      Directive("global main"),
      Label("main"),
      Push(List(FramePointer(), LinkRegister())),
      Mov(FramePointer(), StackPointer()))

    val mainEnd = ListBuffer(
      Mov(Reg(0), Immediate(0)),
      Mov(StackPointer(), FramePointer()),
      Pop(List(FramePointer(), ProgramCounter())))

    ast match {
      case Program(functions, stat) =>
        val statGen = stat.map(s => generate(s))
        mainStart ++ statGen.flatten ++ mainEnd ++ nonMainFunc.values.flatten ++ ListBuffer(Directive("data")) ++ getPrintInstr ++
          ListBuffer(Directive("text"))
      case AssignNew(t, ident, rvalue) =>
        val rv = rvalue.asInstanceOf[Expr]
        exprGen(rv, 8) ++ addVar(ident.name, st.lookup(ident.name).get._1, Reg(8)) ++ ListBuffer(Mov(Reg(0), Reg(8)))
      case Assign(lvalue, rvalue) =>
        val rv = rvalue.asInstanceOf[Expr]
        lvalue match {
          case Ident(a) => exprGen(rv, 8) ++ ListBuffer(Store(Reg(8), getVar(a).get)) ++ ListBuffer(Mov(Reg(0), Reg(8)))
          case _ => ListBuffer()
        }
      case Skip() => ListBuffer()
      case Exit(expr) =>
        val exitGen = exprGen(expr, 8) ++ ListBuffer(Mov(Reg(0), Reg(8)))
        val exit = ListBuffer(Mov(Reg(0), Reg(8)), BranchLink("exit"))
        exitGen ++ exit
      case Print(expr) =>
        val printGen = exprGen(expr, 8) ++ ListBuffer(Mov(Reg(0), Reg(8)))
        expr match {
          case IntLiter(_) =>
            val print = ListBuffer(BranchLink("print_int"))
            nonMainFunc += ("print_int" -> printInt())
            printGen ++ print
          case StrLiter(_) =>
            val print = ListBuffer(BranchLink("print_str"))
            nonMainFunc += ("print_str" -> printString())
            printGen ++ print
          case CharLiter(_) | Chr(_) =>
            val print = ListBuffer(BranchLink("print_char"))
            nonMainFunc += ("print_char" -> printChar())
            printGen ++ print
          case BoolLiter(_) =>
            val print = ListBuffer(BranchLink("print_bool"))
            nonMainFunc += ("print_bool" -> printBool())
            printGen ++ print
          case Add(_, _) | Mul(_, _) | Div(_, _) | Sub(_, _) | Mod(_, _) | Neg(_) | Ord(_) =>
            val print = ListBuffer(BranchLink("print_int"))
            nonMainFunc += ("print_int" -> printInt())
            printGen ++ print
          case And(_, _) | Or(_, _) | Not(_) =>
            val print = ListBuffer(BranchLink("print_bool"))
            nonMainFunc += ("print_bool" -> printBool())
            printGen ++ print
          case Ident(a) =>
            st.lookup(a).get._1 match {
              case IntST() =>
                val print = ListBuffer(BranchLink("print_int"))
                nonMainFunc += ("print_int" -> printInt())
                printGen ++ print
              case BoolST() =>
                val print = ListBuffer(BranchLink("print_bool"))
                nonMainFunc += ("print_bool" -> printBool())
                printGen ++ print
              case CharST() =>
                val print = ListBuffer(BranchLink("print_char"))
                nonMainFunc += ("print_char" -> printChar())
                printGen ++ print
              case StringST() =>
                val print = ListBuffer(BranchLink("print_str"))
                nonMainFunc += ("print_str" -> printString())
                printGen ++ print
              case _ => ListBuffer()
            }
          case _ => ListBuffer()
        }
      case Println(expr) =>
        val normalPrint = generate(Print(expr)(expr.pos))
        nonMainFunc += ("print_ln" -> printLn())
        normalPrint ++ ListBuffer(BranchLink("print_ln"))
      case _ => ListBuffer()
    }
  }

  private def exprGen(expr: Expr, reg: Int): ListBuffer[Instruction] = {
    expr match {
      case Ident(name) =>
        ListBuffer(Load(Reg(reg), getVar(name).get))
      // mov reg, #value doesn't work for huge numbers
      case IntLiter(value) =>
        ListBuffer(Load(Reg(reg), ImmediateJump(Immediate(value))))
      case StrLiter(value) =>
        val label = addStrFun(value)
        ListBuffer(Load(Reg(reg), LabelJump(label)))
      case CharLiter(value) =>
        ListBuffer(Mov(Reg(reg), Immediate(value.toInt)))
      case BoolLiter(value) =>
        ListBuffer(Mov(Reg(reg), Immediate(if (value) 1 else 0)))
      case Not(expr) =>
        val not = ListBuffer(Xor(Reg(reg), Reg(reg), Immediate(1)))
        exprGen(expr, reg) ++ not
      case Neg(expr) =>
        val neg = ListBuffer(RevSub(Reg(reg), Reg(reg), Immediate(0)))
        exprGen(expr, reg) ++ neg
      case Ord(expr) => // do nothing
        exprGen(expr, reg)
      case Chr(expr) => // do nothing
        exprGen(expr, reg)
      case Mul(expr1, expr2) =>
        val mul = ListBuffer(MulInstr(Reg(reg), Reg(reg + 1), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(
          Compare(Reg(reg + 1), Operand2(Reg(reg), "asr", Immediate(31))),
          BranchLinkWithCond("ne", "overflow_error")
        )
        nonMainFunc += ("overflow_error" -> overflowError())
        generateBinOps(reg, expr1, expr2) ++ mul ++ overflow
      case Div(expr1, expr2) =>
        val div = ListBuffer(
          Mov(Reg(0), Reg(reg)),
          Mov(Reg(1), Reg(reg + 1)),
          Compare(Reg(1), Immediate(0)),
          BranchLinkWithCond("eq", "divide_by_zero_error"),
          BranchLink("__aeabi_idivmod"),
          Mov(Reg(reg), Reg(0))
        )
        nonMainFunc += ("divide_by_zero_error" -> divideByZeroError())
        generateBinOps(reg, expr1, expr2) ++ div
      case Mod(expr1, expr2) =>
        val mod = ListBuffer(
          Mov(Reg(0), Reg(reg)),
          Mov(Reg(1), Reg(reg + 1)),
          Compare(Reg(1), Immediate(0)),
          BranchLinkWithCond("eq", "divide_by_zero_error"),
          BranchLink("__aeabi_idivmod"),
          Mov(Reg(reg), Reg(1))
        )
        nonMainFunc += ("divide_by_zero_error" -> divideByZeroError())
        generateBinOps(reg, expr1, expr2) ++ mod
      case Add(expr1, expr2) =>
        val add = ListBuffer(AddInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(BranchLinkWithCond("vs", "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        generateBinOps(reg, expr1, expr2) ++ add ++ overflow
      case Sub(expr1, expr2) =>
        val sub = ListBuffer(SubInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(BranchLinkWithCond("vs", "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        generateBinOps(reg, expr1, expr2) ++ sub ++ overflow
      case And(expr1, expr2) =>
        val and = ListBuffer(AndInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        generateBinOps(reg, expr1, expr2) ++ and
      case Or(expr1, expr2) =>
        val or = ListBuffer(OrInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        generateBinOps(reg, expr1, expr2) ++ or
      case _ => ListBuffer()
    }
  }

  private def generateBinOps(reg: Int, expr1: Expr, expr2: Expr): ListBuffer[Instruction] = {
    val binGen = exprGen(expr1, reg) ++ ListBuffer(Push(List(Reg(reg)))) ++ exprGen(expr2, reg) ++ ListBuffer(Mov(Reg(reg + 1), Reg(reg)), Pop(List(Reg(reg))))
    nonMainFunc += ("print_str" -> printString())
    binGen
  }
}
