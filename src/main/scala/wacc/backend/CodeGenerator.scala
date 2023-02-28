package wacc.backend

import wacc.ast._
import wacc.backend.Instruction._
import wacc.backend.Operand._
import wacc.backend.Print._
import wacc.backend.Stack._
import wacc.backend.Translator.translate
import wacc.frontend.STType._
import wacc.frontend.SymbolTable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CodeGenerator {

  def generateString(listBuffer: ListBuffer[Instruction]): String = {
    listBuffer.map(instr => translate(instr)).mkString ++ "\n"
  }

  private var nonMainFunc: Map[String, ListBuffer[Instruction]] = Map()
  private var functions: List[Func] = List()
  private val generatedFunctions: mutable.Set[String] = mutable.Set()
  private var funcStackIniSize: Int = 0

  var currST: SymbolTable = _
  private var labelCnt = 0

  private def generateFunc(func: Func): ListBuffer[Instruction] = {
    currST = func.symbolTable
    funcStackIniSize = getStackSize
    val stackSetUp = addFrame(currST, isStack = true)
    val funcStart = ListBuffer(
      Label("wacc_" + func.ident.name),
      Push(List(LinkRegister()))
    )
    val eval = func.stat.map(s => generate(s))
    val funcEnd = ListBuffer(
      Pop(List(ProgramCounter())),
      Directive("ltorg")
    )
    nonMainFunc += ("wacc_" + func.ident.name -> (funcStart ++ stackSetUp ++ eval.flatten ++ removeFrame() ++ funcEnd))
    ListBuffer()
  }

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
        val stackSetUp = addFrame(currST, isStack = false)
        this.functions = functions
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
            var isCharArray = false
            currST.lookupAll(ident.name).get._1 match {
              case ArrayST(CharST()) => isCharArray = true
              case _ =>
            }
            if (isCharArray) nonMainFunc += ("array_store_b" -> arrayStoreByte())
            else nonMainFunc += ("array_store" -> arrayStore())
            nonMainFunc += ("bounds_error" -> boundsError())
            val result = ListBuffer[Instruction]()
            result ++= exprGen(exprList.head, 10)
            result ++= rvalueGen(rvalue)
            result += Load(Reg(3), getVar(ident.name).get)
            if (isCharArray) result += BranchLink("array_store_b")
            else result += BranchLink("array_store")
            result
          case fst@FstElem(_) =>
            val result = ListBuffer[Instruction]()
            result ++= pairStore(fst)
            result ++= rvalueGen(rvalue)
            result += Store(Reg(8), RegOffset(Reg(9), Immediate(0)))
            result
          case snd@SndElem(_) =>
            val result = ListBuffer[Instruction]()
            result ++= pairStore(snd)
            result ++= rvalueGen(rvalue)
            result += Store(Reg(8), RegOffset(Reg(9), Immediate(0)))
            result
          case _ => ListBuffer()
        }
      case _if@If(cond, trueStat, falseStat) =>
        val trueLabel = ".L" + labelCnt
        val contLabel = ".L" + (labelCnt + 1)
        labelCnt += 2
        val condGen = exprGen(cond, 8)
        val falseStack = addFrame(_if.falseSymbolTable, isStack = false)
        currST = _if.falseSymbolTable
        // the parent symbol table of "if" (set when checking semantics) should be the old currST before this assignment
        val falseGen = falseStat.map(s => generate(s))
        val falseRemove = removeFrame()
        val trueStack = addFrame(_if.trueSymbolTable, isStack = false)
        currST = _if.trueSymbolTable
        val trueGen = trueStat.map(s => generate(s))
        val trueRemove = removeFrame()
        currST = _if.trueSymbolTable.parentTable.get
        condGen ++ ListBuffer(Compare(Reg(8), Immediate(1)), Branch(Equal, Label(trueLabel))) ++
          falseStack ++ falseGen.flatten ++ falseRemove ++
          ListBuffer(Branch(Nothing, Label(contLabel)), Label(trueLabel)) ++
          trueStack ++ trueGen.flatten ++ trueRemove ++
          ListBuffer(Label(contLabel)) ++ ListBuffer(Move(Reg(0), Immediate(0)))
      case w@While(cond, stat) =>
        val condLabel = ".L" + labelCnt
        val loopLabel = ".L" + (labelCnt + 1)
        labelCnt += 2
        val condGen = exprGen(cond, 8)
        currST = w.symbolTable
        val whileStack = addFrame(w.symbolTable, isStack = false)
        val statGen = stat.map(s => generate(s))
        val whileRemove = removeFrame()
        currST = w.symbolTable.parentTable.get
        ListBuffer(Branch(Nothing, Label(condLabel)), Label(loopLabel)) ++
          whileStack ++ statGen.flatten ++ whileRemove ++
          ListBuffer(Label(condLabel)) ++ condGen ++
          ListBuffer(Compare(Reg(8), Immediate(1)), Branch(Equal, Label(loopLabel))) ++
          ListBuffer(Move(Reg(0), Immediate(0)))
      case bgn@BeginStat(stat) =>
        val stackSetUp = addFrame(bgn.symbolTable, isStack = false)
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
          case PairLiter() =>
            val print = ListBuffer(BranchLink("print_addr"))
            nonMainFunc += ("print_addr" -> printAddr())
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
          case ArrayElem(ident, _) =>
            currST.lookupAll(ident.name).get._1 match {
              case ArrayST(CharST()) =>
                val print = ListBuffer(BranchLink("print_char"))
                nonMainFunc += ("print_char" -> printChar())
                printGen ++ print
              case ArrayST(StringST()) =>
                val print = ListBuffer(BranchLink("print_str"))
                nonMainFunc += ("print_str" -> printString())
                printGen ++ print
              case ArrayST(BoolST()) =>
                val print = ListBuffer(BranchLink("print_bool"))
                nonMainFunc += ("print_bool" -> printBool())
                printGen ++ print
              case _ =>
                val print = ListBuffer(BranchLink("print_int"))
                nonMainFunc += ("print_int" -> printInt())
                printGen ++ print
            }
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
              case ArrayST(CharST()) =>
                val print = ListBuffer(BranchLink("print_str"))
                nonMainFunc += ("print_str" -> printString())
                printGen ++ print
              case ArrayST(_) | PairST(_, _) =>
                val print = ListBuffer(BranchLink("print_addr"))
                nonMainFunc += ("print_addr" -> printAddr())
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
            val result = readTypeHelper(currST.lookupAll(name).get._1)
            if (result.nonEmpty)
              result ++= ListBuffer(Store(Reg(0), getVar(name).get))
            result
          case fst@FstElem(f) =>
            val fstGen = lvalueGen(fst)
            fstGen += Move(Reg(0), Reg(8))
            f match {
              case Ident(name) =>
                val result = readTypeHelper(currST.lookupAll(name).get._1)
                if (result.nonEmpty) {
                  fstGen ++= result
                  fstGen ++= pairStore(fst)
                  fstGen += Move(Reg(8), Reg(12))
                  fstGen += Store(Reg(8), RegOffset(Reg(9), Immediate(0)))
                }
                fstGen
              case _ => ListBuffer()
            }
          case snd@SndElem(s) =>
            val sndGen = lvalueGen(snd)
            sndGen += Move(Reg(0), Reg(8))
            s match {
              case Ident(name) =>
                val result = readTypeHelper(currST.lookupAll(name).get._1)
                if (result.nonEmpty) {
                  sndGen ++= result
                  sndGen ++= pairStore(snd)
                  sndGen += Move(Reg(8), Reg(12))
                  sndGen += Store(Reg(8), RegOffset(Reg(9), Immediate(0)))
                }
                sndGen
              case _ => ListBuffer()
            }
          case _ => ListBuffer()
        }
      case Free(expr) =>
        expr match {
          case Ident(name) =>
            currST.lookupAll(name).get._1 match {
              case PairST(_, _) =>
                nonMainFunc += ("free_pair" -> freePair())
                nonMainFunc += ("null_error" -> nullError())
                nonMainFunc += ("print_str" -> printString())
                val freeGen = exprGen(expr, 8) ++ ListBuffer(Move(Reg(0), Reg(8)))
                val free = ListBuffer(BranchLink("free_pair"))
                freeGen ++ free
              case _ => ListBuffer()
            }
          case _ => ListBuffer()
        }
      case Return(expr) =>
        val result = exprGen(expr, 0)
        var currStackSize = getStackSize
        val clone = getStackClone
        var needPopPC = false
        while (clone.size > funcStackIniSize) {
          needPopPC = true
          result ++= removeFrameClone(clone)
          currStackSize -= 1
        }
        if (needPopPC)
          result ++= ListBuffer(Pop(List(ProgramCounter())))
        else
          result
      case _ => ListBuffer()
    }
  }

  private def readTypeHelper(t: TypeST): ListBuffer[Instruction] = {
    t match {
      case IntST() | PairST(IntST(), _) | PairST(_, IntST()) =>
        nonMainFunc += ("read_int" -> readInt())
        ListBuffer(BranchLink("read_int"))
      case CharST() | PairST(CharST(), _) | PairST(_, CharST()) =>
        nonMainFunc += ("read_char" -> readChar())
        ListBuffer(BranchLink("read_char"))
      case _ => ListBuffer()
    }
  }

  private def rvalueGen(rv: Rvalue): ListBuffer[Instruction] = {
    rv match {
      case expr: Expr => exprGen(expr, 8)
      case ar@ArrayLiter(_) => arrayLiterGen(ar)
      case NewPair(e1, e2) =>
        val result = ListBuffer[Instruction]()
        // fst pair elem
        result ++= newPairElemGen(e1)
        // snd pair elem
        result ++= newPairElemGen(e2)
        // pair
        result += Move(Reg(0), Immediate(8))
        result += BranchLink("malloc")
        result += Move(Reg(12), Reg(0))
        result += Pop(List(Reg(8)))
        result += Store(Reg(8), RegOffset(Reg(12), Immediate(4)))
        result += Pop(List(Reg(8)))
        result += Store(Reg(8), RegOffset(Reg(12), Immediate(0)))
        result += Move(Reg(8), Reg(12))
        result
      case fst@FstElem(_) => lvalueGen(fst)
      case snd@SndElem(_) => lvalueGen(snd)
      case call@Call(ident, exprList) =>
        val result = ListBuffer[Instruction]()
        result ++= addFrame(call.symbolTable, isStack = false)
        val callList = call.symbolTable.getDictNameType.map(_._1)
        for (e <- exprList.indices) {
          result ++= exprGen(exprList(e), 8)
          result ++= storeVar(callList(e), Reg(8))
        }
        if (!generatedFunctions.contains(ident.name)) {
          generatedFunctions += ident.name
          for (f <- functions) {
            if (f.ident.name == ident.name) {
              generateFunc(f)
            }
          }
        }
        result ++= ListBuffer(BranchLink("wacc_" + ident.name))
        result ++= removeFrame()
        result ++= ListBuffer(Move(Reg(8), Reg(0)))
        result
    }
  }

  private def pairStore(lv: Lvalue): ListBuffer[Instruction] = {
    val result = ListBuffer[Instruction]()
    result ++= lvalueGen(lv).dropRight(1)
    result += Push(List(Reg(8)))
    result += Pop(List(Reg(9)))
    result
  }

  private def newPairElemGen(e: Expr): ListBuffer[Instruction] = {
    val result = ListBuffer[Instruction]()
    e match {
      case CharLiter(_) | BoolLiter(_) =>
        result += Move(Reg(0), Immediate(1))
      case _ =>
        result += Move(Reg(0), Immediate(4))
    }
    result += BranchLink("malloc")
    result += Move(Reg(12), Reg(0))
    result ++= exprGen(e, 8)
    result += Store(Reg(8), RegOffset(Reg(12), Immediate(0)))
    result += Move(Reg(8), Reg(12))
    result += Push(List(Reg(8)))
    result
  }

  private def lvalueGen(lv: Lvalue): ListBuffer[Instruction] = {
    val result = ListBuffer[Instruction]()
    lv match {
      case Ident(name) =>
        nonMainFunc += ("null_error" -> nullError())
        nonMainFunc += ("print_str" -> printString())
        result += Load(Reg(8), getVar(name).get)
        result += Compare(Reg(8), Immediate(0))
        result += BranchLinkWithCond(Equal, "null_error")
      case FstElem(lvalue) =>
        result ++= lvalueGen(lvalue)
        result += Load(Reg(8), RegOffset(Reg(8), Immediate(0)))
        result += Load(Reg(8), RegOffset(Reg(8), Immediate(0)))
      case SndElem(lvalue) =>
        result ++= lvalueGen(lvalue)
        result += Load(Reg(8), RegOffset(Reg(8), Immediate(4)))
        result += Load(Reg(8), RegOffset(Reg(8), Immediate(0)))
      case arr@ArrayElem(_, _) => result ++= arrayElemGen(arr)
      case _ =>
    }
    result
  }

  private def arrayLiterGen(array: ArrayLiter): ListBuffer[Instruction] = {
    val result = ListBuffer[Instruction]()
    array.arrayType match {
      case IntST() | StringST() | PairST(_, _) | AnyST() =>
        result += Move(Reg(0), Immediate((array.exprList.length + 1) * 4))
        result ++= arrayLiterSizeHelper(array.exprList.length)
        for (i <- array.exprList.indices) {
          result ++= exprGen(array.exprList(i), 8)
          result += Store(Reg(8), RegOffset(Reg(12), Immediate(i * 4)))
        }
        result += Move(Reg(8), Reg(12))
      case CharST() | BoolST() =>
        result += Move(Reg(0), Immediate(array.exprList.length + 4))
        result ++= arrayLiterSizeHelper(array.exprList.length)
        for (i <- array.exprList.indices) {
          result ++= exprGen(array.exprList(i), 8)
          result += StoreRegByte(Reg(8), RegOffset(Reg(12), Immediate(i)))
        }
        result += Move(Reg(8), Reg(12))
      case ArrayST(IntST()) =>
        result += Move(Reg(0), Immediate((array.exprList.length + 1) * 4))
        result ++= arrayLiterSizeHelper(array.exprList.length)
        for (i <- array.exprList.indices) {
          array.exprList(i) match {
            case Ident(name) =>
              result += Load(Reg(8), getVar(name).get)
            case _ => // ?
          }
          result += Store(Reg(8), RegOffset(Reg(12), Immediate(i * 4)))
        }
        result += Move(Reg(8), Reg(12))
      case _ =>
    }
    result
  }

  private def arrayLiterSizeHelper(length: Int): ListBuffer[Instruction] = {
    ListBuffer(
      BranchLink("malloc"),
      Move(Reg(12), Reg(0)),
      AddInstr(Reg(12), Reg(12), Immediate(4)),
      Load(Reg(9), ImmediateJump(Immediate(length))),
      Store(Reg(9), RegOffset(Reg(12), Immediate(-4))) // length
    )
  }

  private def arrayElemGen(array: ArrayElem): ListBuffer[Instruction] = {
    nonMainFunc += ("bounds_error" -> boundsError())
    val result = ListBuffer[Instruction]()
    var label = "array_load"
    if (arrayElemSizeHelper(currST.lookupAll(array.ident.name).get._1)) {
      label = "array_load_b"
      nonMainFunc += (label -> arrayLoadByte())
    } else
      nonMainFunc += (label -> arrayLoad())
    result += Move(Reg(12), StackPointer())
    result ++= exprGen(array.exprList.head, 10)
    result += Load(Reg(8), getVar(array.ident.name).get)
    result += Push(List(Reg(3)))
    result += Move(Reg(3), Reg(8))
    result += BranchLink(label)
    result += Move(Reg(8), Reg(3))
    result += Pop(List(Reg(3)))
    for (i <- array.exprList.indices.tail) {
      result += Push(List(Reg(8)))
      result ++= exprGen(array.exprList(i), 10)
      result += Pop(List(Reg(8)))
      result += Push(List(Reg(3)))
      result += Move(Reg(3), Reg(8))
      result += BranchLink(label)
      result += Move(Reg(8), Reg(3))
      result += Pop(List(Reg(3)))
    }
    result
  }

  @tailrec
  private def arrayElemSizeHelper(t: TypeST): Boolean = {
    t match {
      case ArrayST(IntST()) | ArrayST(StringST()) | ArrayST(PairST(_, _)) => false
      case ArrayST(CharST()) | ArrayST(BoolST()) => true
      case ArrayST(t) => arrayElemSizeHelper(t)
      case _ => false
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
        // assembly doesn't like " in strings, must be escaped
        val label = addStrFun(value.replace("\"", "\\\"").replace("\n", "\\n"))
        ListBuffer(Load(Reg(reg), LabelJump(label)))
      case CharLiter(value) =>
        ListBuffer(Move(Reg(reg), Immediate(value.toInt)))
      case BoolLiter(value) =>
        ListBuffer(Move(Reg(reg), Immediate(if (value) 1 else 0)))
      case a@ArrayElem(_, _) => arrayElemGen(a)
      case PairLiter() => ListBuffer(Move(Reg(8), Immediate(0)))
      case Not(expr) =>
        val not = ListBuffer(Xor(Reg(reg), Reg(reg), Immediate(1)))
        exprGen(expr, reg) ++ not
      case Neg(expr) =>
        val neg = ListBuffer(RevSub(Reg(reg), Reg(reg), Immediate(0)), BranchLinkWithCond(Overflow, "overflow_error"))
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
          BranchLinkWithCond(NotEqual, "overflow_error")
        )
        nonMainFunc += ("overflow_error" -> overflowError())
        binOpsGen(reg, expr1, expr2) ++ mul ++ overflow
      case Div(expr1, expr2) =>
        val div = ListBuffer(
          Move(Reg(0), Reg(reg)),
          Move(Reg(1), Reg(reg + 1)),
          Compare(Reg(1), Immediate(0)),
          BranchLinkWithCond(Equal, "divide_by_zero_error"),
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
          BranchLinkWithCond(Equal, "divide_by_zero_error"),
          BranchLink("__aeabi_idivmod"),
          Move(Reg(reg), Reg(1))
        )
        nonMainFunc += ("divide_by_zero_error" -> divideByZeroError())
        binOpsGen(reg, expr1, expr2) ++ mod
      case Add(expr1, expr2) =>
        val add = ListBuffer(AddInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(BranchLinkWithCond(Overflow, "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        binOpsGen(reg, expr1, expr2) ++ add ++ overflow
      case Sub(expr1, expr2) =>
        val sub = ListBuffer(SubInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        val overflow = ListBuffer(BranchLinkWithCond(Overflow, "overflow_error"))
        nonMainFunc += ("overflow_error" -> overflowError())
        binOpsGen(reg, expr1, expr2) ++ sub ++ overflow
      case GT(expr1, expr2) =>
        binOpsGen(reg, expr1, expr2) ++ compareExprGen(GreaterThan, LessEqual, reg)
      case GTE(expr1, expr2) =>
        binOpsGen(reg, expr1, expr2) ++ compareExprGen(GreaterEqual, LessThan, reg)
      case LT(expr1, expr2) =>
        binOpsGen(reg, expr1, expr2) ++ compareExprGen(LessThan, GreaterEqual, reg)
      case LTE(expr1, expr2) =>
        binOpsGen(reg, expr1, expr2) ++ compareExprGen(LessEqual, GreaterThan, reg)
      case EQ(expr1, expr2) =>
        binOpsGen(reg, expr1, expr2) ++ compareExprGen(Equal, NotEqual, reg)
      case NEQ(expr1, expr2) =>
        binOpsGen(reg, expr1, expr2) ++ compareExprGen(NotEqual, Equal, reg)
      case And(expr1, expr2) =>
        val and = ListBuffer(AndInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        binOpsGen(reg, expr1, expr2) ++ and
      case Or(expr1, expr2) =>
        val or = ListBuffer(OrInstr(Reg(reg), Reg(reg), Reg(reg + 1)))
        binOpsGen(reg, expr1, expr2) ++ or
      case _ => ListBuffer()
    }
  }

  private def compareExprGen(cond1: Condition, cond2: Condition, reg: Int): ListBuffer[Instruction] = {
    ListBuffer(
      Compare(Reg(reg), Reg(reg + 1)),
      MoveCond(cond1, Reg(reg), Immediate(1)),
      MoveCond(cond2, Reg(reg), Immediate(0))
    )
  }

  // template for expr binary operations
  private def binOpsGen(reg: Int, expr1: Expr, expr2: Expr): ListBuffer[Instruction] = {
    // expr1 -> reg, push reg, expr2 -> reg, reg + 1 = reg, pop reg
    val binGen = exprGen(expr1, reg) ++ ListBuffer(Push(List(Reg(reg)))) ++ exprGen(expr2, reg) ++ ListBuffer(Move(Reg(reg + 1), Reg(reg)), Pop(List(Reg(reg))))
    nonMainFunc += ("print_str" -> printString())
    binGen
  }
}
