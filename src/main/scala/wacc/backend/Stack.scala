package wacc.backend

import wacc.backend.Instruction.{Instruction, Store, SubInstr}
import wacc.backend.Operand.{FramePointer, Immediate, Operand, Reg, RegOffset, StackPointer}
import wacc.frontend.STType._
import wacc.frontend.SymbolTable

import scala.collection.mutable.ListBuffer

object Stack {

  /*
  -0 ------------- <- sp
  -2 |           |
  -4 ------------- <- fp
   */

  private var pointer = 0
  private val variableMap = scala.collection.mutable.Map[String, Operand]()

  def addVar(name: String, t: TypeST): Int = {
    var offset = 0
    t match {
      case IntST() | BoolST() | CharST() | StringST() => offset = 4
      case _ => offset = 4
    }
    val location = RegOffset(FramePointer(), Immediate(pointer))
    variableMap += (name -> location)
    pointer -= offset
    offset
  }

  def storeVar(name: String, reg: Reg): ListBuffer[Instruction] = {
    ListBuffer(Store(reg, variableMap(name)))
  }

  def addVarST(st: SymbolTable): ListBuffer[Instruction] = {
    var totalOffset = 0
    st.getDictNameType.foreach {
      case (name, t) =>
        totalOffset += addVar(name, t)
    }
    val result:ListBuffer[Instruction] = ListBuffer()
    // to solve invalid constant after fixup
    while (totalOffset >= 900) {
      result += SubInstr(StackPointer(), StackPointer(), Immediate(900))
      totalOffset -= 900
    }
    result += SubInstr(StackPointer(), StackPointer(), Immediate(totalOffset))
    result
  }

  def getVar(name: String): Option[Operand] = {
    variableMap.get(name)
  }

}
