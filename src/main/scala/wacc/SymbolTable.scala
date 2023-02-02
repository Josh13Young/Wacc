package wacc

import wacc.STType.TypeST
import wacc.ast.ASTNode

class SymbolTable(parent: Option[SymbolTable]) {

  var parentTable: Option[SymbolTable] = parent
  private var dictionary: Map[String, (TypeST, ASTNode)] = Map()

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

  override def toString: String = {
    dictionary.toString()
  }
}
