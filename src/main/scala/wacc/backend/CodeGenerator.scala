package wacc.backend

import wacc.ast._
import wacc.backend.Instruction._
import wacc.backend.Operand._
import wacc.backend.Print._
import wacc.backend.Stack.{addFrame, getVar, removeFrame, storeVar}
import wacc.backend.Translator.translate
import wacc.frontend.STType.{ArrayST, BoolST, CharST, IntST, StringST}
import wacc.frontend.SymbolTable

import scala.collection.mutable.ListBuffer

object CodeGenerator {

  def generateString(listBuffer: ListBuffer[Instruction]): String = {
    listBuffer.map(instr => translate(instr)).mkString ++ "\n"
  }

  private var nonMainFunc: Map[String, ListBuffer[Instruction]] = Map()

  var currST: SymbolTable = _
  private var labelCnt = 0

  def generate(ast: ASTNode): ListBuffer[Instruction] = {
    val mainStart = ListBuffer(
      Directive("global main"),
      Label("main"),
      Push(List(LinkRegister()))
    )

    val mainEnd = ListBuffer(
      Move(Reg(0), Immediate(0)),
      Pop(List(ProgramCounter())))

    ast match {
      case Program(functions, stat) =>
        val stackSetUp = addFrame(currST)
        val statGen = stat.map(s => generate(s))
        mainStart ++ stackSetUp ++ statGen.flatten ++ removeFrame() ++ mainEnd ++
          nonMainFunc.values.flatten ++
          ListBuffer(Directive("data")) ++ getPrintInstr ++
          ListBuffer(Directive("text"))
      case AssignNew(_, ident, rvalue) =>
        rvalueGen(rvalue) ++
          storeVar(ident.name, Reg(8)) ++
          ListBuffer(Move(Reg(0), Reg(8)))
      case Assign(lvalue, rvalue) =>
        lvalue match {
          case Ident(a) => rvalueGen(rvalue) ++ ListBuffer(Store(Reg(8), getVar(a).get)) ++ ListBuffer(Move(Reg(0), Reg(8)))
          case ArrayElem(ident, exprList) =>
            nonMainFunc += ("array_store" -> arrayStore())
            nonMainFunc += ("bounds_error" -> boundsError())
            val result = ListBuffer[Instruction]()
            result ++= exprGen(exprList.head, 10)
            result ++= rvalueGen(rvalue)
            result += Load(Reg(3), getVar(ident.name).get)
            result += BranchLink("array_store")
            result
          case _ => ListBuffer()
        }
      case _if@If(cond, trueStat, falseStat) =>
        val trueLabel = ".L" + labelCnt
        val contLabel = ".L" + (labelCnt + 1)
        labelCnt += 2
        val condGen = exprGen(cond, 8)
        val falseStack = addFrame(_if.falseSymbolTable)
        currST = _if.falseSymbolTable
        // the parent symbol table of "if" (set when checking semantics) should be the old currST before this assignment
        val falseGen = falseStat.map(s => generate(s))
        val falseRemove = removeFrame()
        val trueStack = addFrame(_if.trueSymbolTable)
        currST = _if.trueSymbolTable
        val trueGen = trueStat.map(s => generate(s))
        val trueRemove = removeFrame()
        currST = _if.trueSymbolTable.parentTable.get
        condGen ++ ListBuffer(Compare(Reg(8), Immediate(1)), Branch("eq", Label(trueLabel))) ++
          falseStack ++ falseGen.flatten ++ falseRemove ++
          ListBuffer(Branch("", Label(contLabel)), Label(trueLabel)) ++
          trueStack ++ trueGen.flatten ++ trueRemove ++
          ListBuffer(Label(contLabel)) ++ ListBuffer(Move(Reg(0), Immediate(0)))
      case w@While(cond, stat) =>
        val condLabel = ".L" + labelCnt
        val loopLabel = ".L" + (labelCnt + 1)
        labelCnt += 2
        val condGen = exprGen(cond, 8)
        currST = w.symbolTable
        val whileStack = addFrame(w.symbolTable)
        val statGen = stat.map(s => generate(s))
        val whileRemove = removeFrame()
        currST = w.symbolTable.parentTable.get
        ListBuffer(Branch("", Label(condLabel)), Label(loopLabel)) ++
          whileStack ++ statGen.flatten ++ whileRemove ++
          ListBuffer(Label(condLabel)) ++ condGen ++
          ListBuffer(Compare(Reg(8), Immediate(1)), Branch("eq", Label(loopLabel))) ++
          ListBuffer(Move(Reg(0), Immediate(0)))
      case bgn@BeginStat(stat) =>
        val stackSetUp = addFrame(bgn.symbolTable)
        currST = bgn.symbolTable
        val statGen = stat.map(s => generate(s))
        currST = bgn.symbolTable.parentTable.get
        stackSetUp ++ statGen.flatten ++ removeFrame() ++ ListBuffer(Move(Reg(0), Immediate(0)))
      case Skip() => ListBuffer()
      case Exit(expr) =>
        val exitGen = exprGen(expr, 8) ++ ListBuffer(Move(Reg(0), Reg(8)))
        val exit = ListBuffer(Move(Reg(0), Reg(8)), BranchLink("exit"))
        exitGen ++ exit
      case Print(expr) =>
        val printGen = exprGen(expr, 8) ++ ListBuffer(Move(Reg(0), Reg(8)))
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
          case Add(_, _) | Mul(_, _) | Div(_, _) | Sub(_, _) | Mod(_, _) | Neg(_) | Ord(_) | Len(_) =>
            val print = ListBuffer(BranchLink("print_int"))
            nonMainFunc += ("print_int" -> printInt())
            printGen ++ print
          case GT(_, _) | GTE(_, _) | LT(_, _) | LTE(_, _) | EQ(_, _) | NEQ(_, _) =>
            val print = ListBuffer(BranchLink("print_bool"))
            nonMainFunc += ("print_bool" -> printBool())
            printGen ++ print
          case And(_, _) | Or(_, _) | Not(_) =>
            val print = ListBuffer(BranchLink("print_bool"))
            nonMainFunc += ("print_bool" -> printBool())
            printGen ++ print
          case ArrayElem(_, _) =>
            val print = ListBuffer(BranchLink("print_int"))
            nonMainFunc += ("print_int" -> printInt())
            printGen ++ print
          case Ident(a) =>
            currST.lookupAll(a).get._1 match {
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
      case Read(expr) =>
        expr match {
          case Ident(name) =>
            currST.lookupAll(name).get._1 match {
              case IntST() =>
                println("read int")
                nonMainFunc += ("read_int" -> readInt())
                ListBuffer(BranchLink("read_int"), Store(Reg(0), getVar(name).get))
              case CharST() =>
                println("read char")
                nonMainFunc += ("read_char" -> readChar())
                ListBuffer(BranchLink("read_char"), Store(Reg(0), getVar(name).get))
              case _ => ListBuffer()
            }
          case _ => ListBuffer()
        }
      case _ => ListBuffer()
    }
  }

  private def rvalueGen(rv: Rvalue): ListBuffer[Instruction] = {
    rv match {
      case expr: Expr => exprGen(expr, 8)
      case ar@ArrayLiter(_) => arrayLiterGen(ar)
      case NewPair(_, _) => ListBuffer()
      case FstElem(_) => ListBuffer()
      case SndElem(_) => ListBuffer()
      case Call(_, _) => ListBuffer()
    }
  }

  private def arrayLiterGen(array: ArrayLiter): ListBuffer[Instruction] = {
    val result = ListBuffer[Instruction]()
    array.arrayType match {
      case IntST() =>
        result += Move(Reg(0), Immediate((array.exprList.length + 1) * 4))
        result += BranchLink("malloc")
        result += Move(Reg(12), Reg(0))
        result += AddInstr(Reg(12), Reg(12), Immediate(4))
        result += Load(Reg(9), ImmediateJump(Immediate(array.exprList.length)))
        result += Store(Reg(9), RegOffset(Reg(12), Immediate(-4))) // length
        for (i <- array.exprList.indices) {
          result ++= exprGen(array.exprList(i), 8)
          result += Store(Reg(8), RegOffset(Reg(12), Immediate(i * 4)))
        }
        result += Move(Reg(8), Reg(12))
      case ArrayST(IntST()) =>
        result += Move(Reg(0), Immediate((array.exprList.length + 1) * 4))
        result += BranchLink("malloc")
        result += Move(Reg(12), Reg(0))
        result += AddInstr(Reg(12), Reg(12), Immediate(4))
        result += Load(Reg(9), ImmediateJump(Immediate(array.exprList.length)))
        result += Store(Reg(9), RegOffset(Reg(12), Immediate(-4))) // length
        for (i <- array.exprList.indices) {
          array.exprList(i) match {
            case Ident(name) =>
              result += Load(Reg(8), getVar(name).get)
          }
          result += Store(Reg(8), RegOffset(Reg(12), Immediate(i * 4)))
        }
        result += Move(Reg(8), Reg(12))
      case _ =>
    }
    result
  }

  private def arrayElemGen(array: ArrayElem): ListBuffer[Instruction] = {
    nonMainFunc += ("array_load" -> arrayLoad())
    nonMainFunc += ("bounds_error" -> boundsError())
    val result = ListBuffer[Instruction]()
    result += Move(Reg(12), StackPointer())
    result ++= exprGen(array.exprList.head, 10)
    result += Load(Reg(8), getVar(array.ident.name).get)
    result += Push(List(Reg(3)))
    result += Move(Reg(3), Reg(8))
    result += BranchLink("array_load")
    result += Move(Reg(8), Reg(3))
    result += Pop(List(Reg(3)))
    for (i <- array.exprList.indices.tail) {
      result += Push(List(Reg(8)))
      result ++= exprGen(array.exprList(i), 10)
      result += Pop(List(Reg(8)))
      result += Push(List(Reg(3)))
      result += Move(Reg(3), Reg(8))
      result += BranchLink("array_load")
      result += Move(Reg(8), Reg(3))
      result += Pop(List(Reg(3)))
    }
    result
  }

  private def exprGen(expr: Expr, reg: Int): ListBuffer[Instruction] = {
    expr match {
      case Ident(name) =>
        ListBuffer(Load(Reg(reg), getVar(name).get))
      // mov reg, #value doesn't work for huge numbers
      case IntLiter(value) =>
        ListBuffer(Load(Reg(reg), ImmediateJump(Immediate(value))))
      case StrLiter(value) =>
        // assembly doesn't like " in strings, must be escaped
        val label = addStrFun(value.replace("\"", "\\\""))
        ListBuffer(Load(Reg(reg), LabelJump(label)))
      case CharLiter(value) =>
        ListBuffer(Move(Reg(reg), Immediate(value.toInt)))
      case BoolLiter(value) =>
        ListBuffer(Move(Reg(reg), Immediate(if (value) 1 else 0)))
      case a@ArrayElem(_, _) => arrayElemGen(a)
      case Not(expr) =>
        val not = ListBuffer(Xor(Reg(reg), Reg(reg), Immediate(1)))
        exprGen(expr, reg) ++ not
      case Neg(expr) =>
        val neg = ListBuffer(RevSub(Reg(reg), Reg(reg), Immediate(0)), BranchLinkWithCond("vs", "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        nonMainFunc += ("print_str" -> printString()) // for printing error message
        exprGen(expr, reg) ++ neg
      case Len(expr) =>
        val len = ListBuffer(Load(Reg(reg), RegOffset(Reg(reg), Immediate(-4))))
        exprGen(expr, reg) ++ len
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
        binOpsGen(reg, expr1, expr2) ++ mul ++ overflow
      case Div(expr1, expr2) =>
        val div = ListBuffer(
          Move(Reg(0), Reg(reg)),
          Move(Reg(1), Reg(reg + 1)),
          Compare(Reg(1), Immediate(0)),
          BranchLinkWithCond("eq", "divide_by_zero_error"),
          BranchLink("__aeabi_idivmod"),
          Move(Reg(reg), Reg(0))
        )
        nonMainFunc += ("divide_by_zero_error" -> divideByZeroError())
        binOpsGen(reg, expr1, expr2) ++ div
      case Mod(expr1, expr2) =>
        val mod = ListBuffer(
          Move(Reg(0), Reg(reg)),
          Move(Reg(1), Reg(reg + 1)),
          Compare(Reg(1), Immediate(0)),
          BranchLinkWithCond("eq", "divide_by_zero_error"),
          BranchLink("__aeabi_idivmod"),
          Move(Reg(reg), Reg(1))
        )
        nonMainFunc += ("divide_by_zero_error" -> divideByZeroError())
        binOpsGen(reg, expr1, expr2) ++ mod
      case Add(expr1, expr2) =>
        val add = ListBuffer(AddInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(BranchLinkWithCond("vs", "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        binOpsGen(reg, expr1, expr2) ++ add ++ overflow
      case Sub(expr1, expr2) =>
        val sub = ListBuffer(SubInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(BranchLinkWithCond("vs", "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        binOpsGen(reg, expr1, expr2) ++ sub ++ overflow
      case GT(expr1, expr2) =>
        val gt = ListBuffer(Compare(Reg(reg), Reg(reg + 1)), MoveCond("gt", Reg(reg), Immediate(1)), MoveCond("le", Reg(reg), Immediate(0)))
        binOpsGen(reg, expr1, expr2) ++ gt
      case GTE(expr1, expr2) =>
        val gte = ListBuffer(Compare(Reg(reg), Reg(reg + 1)), MoveCond("ge", Reg(reg), Immediate(1)), MoveCond("lt", Reg(reg), Immediate(0)))
        binOpsGen(reg, expr1, expr2) ++ gte
      case LT(expr1, expr2) =>
        val lt = ListBuffer(Compare(Reg(reg), Reg(reg + 1)), MoveCond("lt", Reg(reg), Immediate(1)), MoveCond("ge", Reg(reg), Immediate(0)))
        binOpsGen(reg, expr1, expr2) ++ lt
      case LTE(expr1, expr2) =>
        val lte = ListBuffer(Compare(Reg(reg), Reg(reg + 1)), MoveCond("le", Reg(reg), Immediate(1)), MoveCond("gt", Reg(reg), Immediate(0)))
        binOpsGen(reg, expr1, expr2) ++ lte
      case EQ(expr1, expr2) =>
        val eq = ListBuffer(Compare(Reg(reg), Reg(reg + 1)), MoveCond("eq", Reg(reg), Immediate(1)), MoveCond("ne", Reg(reg), Immediate(0)))
        binOpsGen(reg, expr1, expr2) ++ eq
      case NEQ(expr1, expr2) =>
        val neq = ListBuffer(Compare(Reg(reg), Reg(reg + 1)), MoveCond("ne", Reg(reg), Immediate(1)), MoveCond("eq", Reg(reg), Immediate(0)))
        binOpsGen(reg, expr1, expr2) ++ neq
      case And(expr1, expr2) =>
        val and = ListBuffer(AndInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        binOpsGen(reg, expr1, expr2) ++ and
      case Or(expr1, expr2) =>
        val or = ListBuffer(OrInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        binOpsGen(reg, expr1, expr2) ++ or
      case _ => ListBuffer()
    }
  }

  private def binOpsGen(reg: Int, expr1: Expr, expr2: Expr): ListBuffer[Instruction] = {
    // expr1 -> reg, push reg, expr2 -> reg, reg + 1 = reg, pop reg
    val binGen = exprGen(expr1, reg) ++ ListBuffer(Push(List(Reg(reg)))) ++ exprGen(expr2, reg) ++ ListBuffer(Move(Reg(reg + 1), Reg(reg)), Pop(List(Reg(reg))))
    nonMainFunc += ("print_str" -> printString())
    binGen
  }
}
