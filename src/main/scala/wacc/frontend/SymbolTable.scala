package wacc.frontend

import wacc.ast.ASTNode
import wacc.frontend.STType.TypeST

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SymbolTable(parent: Option[SymbolTable]) {

  var parentTable: Option[SymbolTable] = parent
  private val dictionary: mutable.Map[String, (TypeST, ASTNode)] = mutable.LinkedHashMap[String, (TypeST, ASTNode)]()
  private val childFunctions: ListBuffer[Map[String, SymbolTable]] = ListBuffer()
  var isFunctionBody: Boolean = false
  var functionReturnType: Option[TypeST] = None

  def add(name: String, t: TypeST, node: ASTNode): Unit = {
    dictionary += (name -> (t, node))
  }

  // only look up in current symbol table
  def lookup(name: String): Option[(TypeST, ASTNode)] = {
    dictionary.get(name)
  }

  // Look up all parenting symbol tables
  def lookupAll(name: String): Option[(TypeST, ASTNode)] = {
    var currST = Option(this)
    while (currST.isDefined) {
      currST.get.lookup(name) match {
        case Some(x) => return Some(x)
        case None => currST = currST.get.parentTable
      }
    }
    None
  }

  def addChildFunc(name: String, st: SymbolTable): Unit = {
    childFunctions += Map(name -> st)
  }

  // Same idea as lookupAll but get the symbol table instead of the type
  def locateST(name: String): Option[SymbolTable] = {
    var currST = Option(this)
    while (currST.isDefined) {
      currST.get.lookup(name) match {
        case Some(x) => return currST
        case None => currST = currST.get.parentTable
      }
    }
    None
  }

  def getChildFunc(name: String): SymbolTable = {
    var symbolTable = new SymbolTable(None)
    for (func <- childFunctions) {
      val result = func.find(_._1 == name)
      if (result.isDefined) {
        symbolTable = result.get._2
      }
    }
    symbolTable
  }

  def dictToList(): List[(TypeST, ASTNode)] = {
    dictionary.toList.map(x => (x._2._1, x._2._2))
  }
}
