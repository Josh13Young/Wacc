package wacc.backend

import wacc.backend.Instruction.{AddInstr, Instruction, Move, Pop, Push, Store, SubInstr}
import wacc.backend.Operand.{FramePointer, Immediate, Operand, Reg, RegOffset, StackPointer}
import wacc.frontend.STType._
import wacc.frontend.SymbolTable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Stack {

  private val stack = scala.collection.mutable.Stack[StackFrame]()

  class StackFrame(st: SymbolTable) {
    val variableMap: mutable.Map[String, Operand] = scala.collection.mutable.Map[String, Operand]()
    val variableDefined: mutable.Set[String] = scala.collection.mutable.Set[String]()
    var pointer = 0
  }

  private def addVar(name: String, t: TypeST, sf: StackFrame): Unit = {
    var offset = 0
    t match {
      case IntST() | BoolST() | CharST() | StringST() => offset = 4
      case _ => offset = 4
    }
    val location = RegOffset(FramePointer(), Immediate(sf.pointer))
    sf.variableMap += (name -> location)
    sf.pointer += offset
  }

  def storeVar(name: String, reg: Reg): ListBuffer[Instruction] = {
    val sf = stack.top
    sf.variableDefined += name
    ListBuffer(Store(reg, sf.variableMap(name)))
  }

  private def addVarST(st: SymbolTable, sf: StackFrame): ListBuffer[Instruction] = {
    st.getDictNameType.foreach {
      case (name, t) =>
        addVar(name, t, sf)
    }
    val result: ListBuffer[Instruction] = ListBuffer()
    // to solve invalid constant after fixup
    var totalOffset = sf.pointer
    while (totalOffset >= 900) {
      result += SubInstr(StackPointer(), StackPointer(), Immediate(900))
      totalOffset -= 900
    }
    result += SubInstr(StackPointer(), StackPointer(), Immediate(totalOffset))
    result
  }

  def addFrame(st: SymbolTable): ListBuffer[Instruction] = {
    val sf = new StackFrame(st)
    stack.push(sf)
    ListBuffer(Push(List(FramePointer()))) ++ addVarST(st, sf) ++ ListBuffer(Move(FramePointer(), StackPointer()))
  }

  def removeFrame(st: SymbolTable): ListBuffer[Instruction] = {
    val sf = stack.pop()
    var totalOffset = sf.pointer
    val result: ListBuffer[Instruction] = ListBuffer()
    while (totalOffset >= 900) {
      result += AddInstr(StackPointer(), StackPointer(), Immediate(900))
      totalOffset -= 900
    }
    result += AddInstr(StackPointer(), StackPointer(), Immediate(totalOffset))
    result += Pop(List(FramePointer()))
    result
  }

  def getVar(name: String): Option[Operand] = {
    val stackClone = stack.clone()
    var fpOffset = 0
    while (stackClone.nonEmpty) {
      val sf = stackClone.pop()
      if (sf.variableDefined.contains(name)) {
        sf.variableMap(name) match {
          case RegOffset(fp, Immediate(offset)) => return Some(RegOffset(fp, Immediate(offset + fpOffset)))
          case _ => return None // Not reachable
        }
      }
      fpOffset += sf.pointer + 4 // 4 for the poped fp
    }
    None // Not reachable
  }
}
