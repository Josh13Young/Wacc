package wacc.backend

import wacc.backend.Instruction.{AddInstr, Instruction, Move, Pop, Push, Store, SubInstr}
import wacc.backend.Operand.{FramePointer, Immediate, Operand, Reg, RegOffset, StackPointer}
import wacc.frontend.SymbolTable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Stack {

  private val stack = scala.collection.mutable.Stack[StackFrame]()

  class StackFrame {
    // map from variable name to its location
    val variableMap: mutable.Map[String, Operand] = scala.collection.mutable.Map[String, Operand]()
    // set of variables that are stored in stack by storeVar
    val variableDefined: mutable.Set[String] = scala.collection.mutable.Set[String]()
    // position from frame pointer
    var pointer = 0
    // whether this stack frame is for function
    var isFuncStack = false
  }

  // add a new variable to stack frame
  private def addVar(name: String, sf: StackFrame): Unit = {
    // offset is 4 for all data types including bool and char, a bit wasteful but easy to manage
    val offset = 4
    val location = RegOffset(FramePointer(), Immediate(sf.pointer))
    sf.variableMap += (name -> location)
    sf.pointer += offset
  }

  // store variable to its location in stack frame
  def storeVar(name: String, reg: Reg): ListBuffer[Instruction] = {
    val sf = stack.top
    sf.variableDefined += name
    ListBuffer(Store(reg, sf.variableMap(name)))
  }

  // add variables in symbol table to stack frame, also allocate space for stack
  private def addVarST(st: SymbolTable, sf: StackFrame): ListBuffer[Instruction] = {
    st.getDictName.foreach(name => addVar(name, sf))
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

  // allocate new stack frame and add variables to it
  def addFrame(st: SymbolTable, isFuncStack: Boolean): ListBuffer[Instruction] = {
    val sf = new StackFrame
    stack.push(sf)
    sf.isFuncStack = isFuncStack
    val result: ListBuffer[Instruction] = ListBuffer()
    result += Push(List(FramePointer()))
    result ++= addVarST(st, sf)
    result += Move(FramePointer(), StackPointer())
    result
  }

  // remove frame by popping stack frame and add back the stack pointer to free the memory
  def removeFrame(): ListBuffer[Instruction] = {
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

  // same as above but no popping, used for function early return, proper stack frame is popped at the end of function
  def removeFrameClone(clone: mutable.Stack[StackFrame]): ListBuffer[Instruction] = {
    val sf = clone.pop()
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

  // find the location of a variable in the stack
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
      fpOffset += sf.pointer + 4 // 4 for extra fp
      if (sf.isFuncStack) {
        fpOffset += 4 // 4 more for extra pc
      }
    }
    None // Not reachable
  }

  def getStackClone: mutable.Stack[StackFrame] = {
    stack.clone()
  }

  def getStackSize: Int = {
    stack.size
  }
}
