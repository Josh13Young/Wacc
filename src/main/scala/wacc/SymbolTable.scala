package wacc

import wacc.STType.TypeST
import wacc.ast.ASTNode
import scala.collection.mutable.ListBuffer

class SymbolTable(parent: Option[SymbolTable]) {

  var parentTable: Option[SymbolTable] = parent
  var dictionary: Map[String, (TypeST, ASTNode)] = Map()
  var childFunctions: ListBuffer[Map[String, SymbolTable]] = ListBuffer()

  def add(name: String, t: TypeST, node: ASTNode): Unit = {
    dictionary += (name -> (t, node))
  }

  def lookup(name: String): Option[(TypeST, ASTNode)] = {
    dictionary.get(name)
  }

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

  def dictToList(dict: Map[String, (TypeST, ASTNode)]): List[(TypeST, ASTNode)] = {
    dict.toList.map(x => (x._2._1, x._2._2))
  }

  override def toString: String = {
    dictionary.toString()
  }
}
