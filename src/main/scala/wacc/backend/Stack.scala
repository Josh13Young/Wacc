package wacc.backend

import wacc.backend.Instruction.{Instruction, Store, SubInstr}
import wacc.backend.Operand.{FramePointer, Immediate, Operand, Reg, RegOffset, StackPointer}
import wacc.frontend.STType._

import scala.collection.mutable.ListBuffer

object Stack {

  /*
  -0 ------------- <- sp
  -2 |           |
  -4 ------------- <- fp
   */

  private var pointer = 0
  private val variableMap = scala.collection.mutable.Map[String, Operand]()

  def addVar(name: String, t: TypeST, reg: Reg): ListBuffer[Instruction] = {

    var offset = 0
    t match {
      case IntST() | BoolST() | CharST() | StringST() => offset = 4
      case _ => offset = 4
    }
    val location = RegOffset(FramePointer(), Immediate(pointer))
    variableMap += (name -> location)
    pointer -= offset
    ListBuffer(SubInstr(StackPointer(), StackPointer(), Immediate(offset)), Store(reg, location))
  }

  def getVar(name: String): Option[Operand] = {
    variableMap.get(name)
  }

}
