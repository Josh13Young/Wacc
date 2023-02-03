package wacc

import parsley.Parsley
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.position.pos
import wacc.STType._

object ast {

  // Generic Bridge Traits
  trait ParserBridgePos[+A] {
    def con(pos: (Int, Int)): A

    final def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con) <~ op
  }

  trait ParserBridgePos0[+A] extends ParserBridgePos[A] {
    def apply()(pos: (Int, Int)): A

    override def con(pos: (Int, Int)): A = apply()(pos)
  }

  trait ParserBridgePos1[-A, +B] extends ParserBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(apply(_) _)

    override def con(pos: (Int, Int)): A => B = apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C] extends ParserBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C

    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] = pos <**> (x, y).zipped(apply(_, _) _)

    override def con(pos: (Int, Int)): (A, B) => C = apply(_, _)(pos)
  }

  // node that holds position information (line, column)
  sealed trait ASTNode {
    val pos: (Int, Int)
  }

  // <PROGRAM>
  // Since Stat; Stat is valid, we represent it as a list of Stat
  case class Program(functions: List[Func], stat: List[Stat])(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable) : Boolean = {
      var result = true
      println("Checking program: ")
      for (f <- functions) {
        if (!f.check(st)) {
          result = false
        }
      }
      for (s <- stat) {
        if (!s.check(st)) {
          result = false
        }
      }
      println("Gucci")
      result
    }
  }

  object Program {
    def apply(functions: Parsley[List[Func]], stat: Parsley[List[Stat]]): Parsley[Program] =
      pos <**> (functions, stat).zipped(Program(_, _) _)
  }

  // <FUNC>

  case class Func(t: Type, ident: Ident, vars: List[Param], stat: List[Stat])(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable): Boolean = {
      println("Checking function: " + ident.name)
      val query = st.lookup(ident.name)
      if (query.isDefined) {
        println("Error: function " + ident.name + " already defined\n")
        return false
      }
      st.add(ident.name, t.getType(st), this)
      val funST = new SymbolTable(Some(st))
      funST.parentTable = Some(st)
      for (v <- vars) {
        if (!v.check(funST)) {
          return false
        }
        funST.add(v.ident.name, v.getType(st), v)
      }
      println(funST)
      for (s <- stat) {
        if (!s.check(funST)) {
          return false
        }
      }
      st.childFunctions += Map(ident.name -> funST)
      println(st.childFunctions.toList)
      println(st)
      true
    }
  }

  object Func {
    def apply(t: Parsley[Type], ident: Parsley[Ident], vars: Parsley[List[Param]], stat: Parsley[List[Stat]]): Parsley[Func] =
      pos <**> (t, ident, vars, stat).zipped(Func(_, _, _, _) _)
  }

  // <PARAM-LIST> omitted (only 1 occurrence)

  // <PARAM>
  case class Param(t: Type, ident: Ident)(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable): Boolean = {
      println("Checking param: " + ident.name)
      val query = st.lookup(ident.name)
      if (query.isDefined) {
        println("Error: " + ident.name + " already defined\n")
        return false
      }
      true
    }
    def getType(st: SymbolTable): TypeST = t.getType(st)
  }

  object Param {
    def apply(t: Parsley[Type], ident: Parsley[Ident]): Parsley[Param] =
      pos <**> (t, ident).zipped(Param(_, _) _)
  }

  // <STAT>
  sealed trait Stat extends ASTNode {
    def check(st: SymbolTable): Boolean
  }

  case class Skip()(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking skip")
      true
    }
  }

  object Skip extends ParserBridgePos0[Skip]

  case class AssignNew(t: Type, ident: Ident, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking assign new: " + ident.name)
      val query = st.lookup(ident.name)
      if (query.isDefined) {
        println("Error: " + ident.name + " already defined\n")
        return false
      }
      val rhsType = rvalue.getType(st)
      val lhsType = t.getType(st)
      println("LHS type:" + lhsType + " RHS type: " + rhsType)
      val resType = typeCompare(lhsType, rhsType)
      if (!typeCheck(resType)) {
        println("Error: " + ident.name + " type mismatch\n")
        false
      } else {
        if (!rvalue.check(st)) {
          println("Error: " + ident.name + " RHS check failed\n")
          return false
        }
        st.add(ident.name, resType, rvalue)
        println(st)
        println("Assign New Gucci\n")
        true
      }
    }
  }

  object AssignNew {
    def apply(t: Parsley[Type], ident: Parsley[Ident], rvalue: Parsley[Rvalue]): Parsley[AssignNew] =
      pos <**> (t, ident, rvalue).zipped(AssignNew(_, _, _) _)
  }

  case class Assign(lvalue: Lvalue, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking assign: " + lvalue)
      if (!lvalue.check(st)) {
        println("Error: " + lvalue + " LHS check failed\n")
        return false
      }
      val identType = lvalue.getType(st)
      val rhsType = rvalue.getType(st)
      println("LHS type:" + identType + " RHS type: " + rhsType)
      val resType = typeCompare(identType, rhsType)
      if (!typeCheck(resType)) {
        println("Error: " + lvalue + " type mismatch\n")
        return false
      }
      if (!rvalue.check(st)) {
        println("Error: " + lvalue + " RHS check failed\n")
        return false
      }
      true
    }
  }

  object Assign {
    def apply(lvalue: Parsley[Lvalue], rvalue: Parsley[Rvalue]): Parsley[Assign] =
      pos <**> (lvalue, rvalue).zipped(Assign(_, _) _)
  }

  case class Read(lvalue: Lvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking read: " + lvalue)
      //TODO
      true
    }
  }

  object Read {
    def apply(lvalue: Parsley[Lvalue]): Parsley[Read] =
      pos <**> lvalue.map(Read(_) _)
  }

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking free: " + expr)
      expr.getType(st) match {
        case ArrayST(_) => expr.check(st)
        case PairST(_, _) => expr.check(st)
        case _ =>
          println("Error: " + expr + " is not an array or pair\n")
          false
      }
    }
  }

  object Free extends ParserBridgePos1[Expr, Free]

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking return: " + expr)
      //TODO
      true
    }
  }

  object Return extends ParserBridgePos1[Expr, Return]

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking exit: " + expr)
      expr.getType(st) == IntST() && expr.check(st)
    }
  }

  object Exit extends ParserBridgePos1[Expr, Exit]

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking print: " + expr)
      expr.check(st)
    }
  }

  object Print extends ParserBridgePos1[Expr, Print]

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking println: " + expr)
      expr.check(st)
    }
  }

  object Println extends ParserBridgePos1[Expr, Println]

  // Stat can call another Stat, so we represent it as a list of Stat
  case class If(cond: Expr, trueStat: List[Stat], falseStat: List[Stat])(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking if: " + cond + "...")
      if (cond.getType(st) != BoolST()) {
        println("Error: " + cond + " is not a boolean\n")
        return false
      }
      if (!cond.check(st)) {
        println("Error: " + cond + " check failed\n")
        return false
      }
      val trueST = new SymbolTable(Option(st))
      if (!trueStat.forall(_.check(trueST))) {
        println("Error: " + trueStat + " check failed\n")
        return false
      }
      val falseST = new SymbolTable(Option(st))
      if (!falseStat.forall(_.check(falseST))) {
        println("Error: " + falseStat + " check failed\n")
        return false
      }
      true
    }
  }

  object If {
    def apply(cond: Parsley[Expr], trueStat: Parsley[List[Stat]], falseStat: Parsley[List[Stat]]): Parsley[If] =
      pos <**> (cond, trueStat, falseStat).zipped(If(_, _, _) _)
  }

  case class While(cond: Expr, stat: List[Stat])(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking while: " + cond + "...")
      if (cond.getType(st) != BoolST()) {
        println("Error: " + cond + " is not a boolean\n")
        return false
      }
      if (!cond.check(st)) {
        println("Error: " + cond + " check failed\n")
        return false
      }
      val whileST = new SymbolTable(Option(st))
      if (!stat.forall(_.check(whileST))) {
        println("Error: " + stat + " check failed\n")
        return false
      }
      true
    }
  }

  object While {
    def apply(cond: Parsley[Expr], stat: Parsley[List[Stat]]): Parsley[While] =
      pos <**> (cond, stat).zipped(While(_, _) _)
  }

  case class BeginStat(stat: List[Stat])(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable): Boolean = {
      println("Checking begin: " + stat + "...")
      val beginST = new SymbolTable(Option(st))
      if (!stat.forall(_.check(beginST))) {
        println("Error: " + stat + " check failed\n")
        return false
      }
      true
    }
  }

  object BeginStat {
    def apply(stat: Parsley[List[Stat]]): Parsley[BeginStat] =
      pos <**> stat.map(BeginStat(_) _)
  }

  // <LVALUE>
  sealed trait Lvalue extends ASTNode {
    def check(st: SymbolTable): Boolean

    def getType(st: SymbolTable): TypeST
  }

  // <IDENT>
  case class Ident(name: String)(val pos: (Int, Int)) extends Lvalue with Expr {
    def check(st: SymbolTable): Boolean = {
      println("Checking ident: " + name)
      val query = st.lookupAll(name)
      if (query.isEmpty) {
        println("Error: " + name + " is not declared")
        false
      } else {
        println("Ident " + name + " is declared")
        true
      }
    }

    override def getType(st: SymbolTable): TypeST = {
      val query = st.lookupAll(name)
      if (query.isEmpty) {
        println("Error: " + name + " is not declared (type)")
        null
      } else {
        println("Ident " + name + " is declared (type)")
        query.head._1
      }
    }
  }

  object Ident {
    def apply(name: Parsley[String]): Parsley[Ident] =
      pos <**> name.map(Ident(_) _)
  }

  // <ARRAY-ELEM>
  case class ArrayElem(ident: Ident, exprList: List[Expr])(val pos: (Int, Int)) extends Lvalue with Expr {
    override def check(st: SymbolTable): Boolean = {
      var result = true
      println("Checking array elem: " + ident)
      ident.check(st)
      for (expr <- exprList) {
        if (expr.getType(st) != IntST()) {
          println("Error: array index must be of type int")
          result = false
        }
        expr.check(st)
      }
      result
    }

    override def getType(st: SymbolTable): TypeST = {
      println("Getting type of array elem: " + ident)
      var identType = ident.getType(st)
      println("Ident type: " + identType)
      for (i <- 1 to exprList.length) {
        identType match {
          case ArrayST(t) => identType = t
          case _ =>
            println("Error: " + ident + " is not an array")
            identType = null
        }
      }
      identType
    }
  }

  object ArrayElem {
    def apply(ident: Parsley[Ident], exprList: Parsley[List[Expr]]): Parsley[ArrayElem] =
      pos <**> (ident, exprList).zipped(ArrayElem(_, _) _)
  }

  // <PAIR-ELEM>
  sealed trait PairElem extends Lvalue with Rvalue

  case class FstElem(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem {
    override def check(st: SymbolTable): Boolean = {
      println("Checking fst of : " + lvalue)
      lvalue.getType(st) match {
        case PairST(_, _) =>
          lvalue.check(st)
        case _ =>
          println("Error: " + lvalue + " is not a pair")
          false
      }
    }

    override def getType(st: SymbolTable): TypeST = {
      println("Getting type of fst of : " + lvalue)
      lvalue.getType(st) match {
        case PairST(t1, _) => t1
        case _ =>
          println("Error: " + lvalue + " is not a pair")
          VoidST()
      }
    }
  }

  object FstElem {
    def apply(lvalue: Parsley[Lvalue]): Parsley[FstElem] =
      pos <**> lvalue.map(FstElem(_) _)
  }

  case class SndElem(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem {
    override def check(st: SymbolTable): Boolean = {
      println("Checking snd of : " + lvalue)
      lvalue.getType(st) match {
        case PairST(_, _) =>
          lvalue.check(st)
        case _ =>
          println("Error: " + lvalue + " is not a pair")
          false
      }
    }

    override def getType(st: SymbolTable): TypeST = {
      println("Getting type of snd of : " + lvalue)
      lvalue.getType(st) match {
        case PairST(_, t2) => t2
        case _ =>
          println("Error: " + lvalue + " is not a pair")
          VoidST()
      }
    }
  }

  object SndElem {
    def apply(lvalue: Parsley[Lvalue]): Parsley[SndElem] =
      pos <**> lvalue.map(SndElem(_) _)
  }

  // <RVALUE>
  sealed trait Rvalue extends ASTNode {
    def check(st: SymbolTable): Boolean

    def getType(st: SymbolTable): TypeST
  }

  // <ARRAY-LITER>
  case class ArrayLiter(exprList: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable): Boolean = {
      println("Checking array liter")
      var result = true
      if (exprList.isEmpty) {
        println("Success, empty array")
      } else {
        val elemType = exprList.head.getType(st)
        for (expr <- exprList) {
          if (expr.getType(st) != elemType) {
            println("Error: array elements must be of the same type")
            result = false
          }
          expr.check(st)
        }
      }
      result
    }

    override def getType(st: SymbolTable): TypeST = {
      println("Getting type of array liter")
      if (exprList.isEmpty) {
        ArrayST(AnyST())
      } else {
        val exprType = exprList.head.getType(st)
        ArrayST(exprType)
      }
    }
  }

  object ArrayLiter {
    def apply(exprList: Parsley[List[Expr]]): Parsley[ArrayLiter] =
      pos <**> exprList.map(ArrayLiter(_) _)
  }

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable): Boolean = {
      println("Checking new pair")
      expr1.check(st) && expr2.check(st)
    }

    override def getType(st: SymbolTable): TypeST = {
      PairST(expr1.getType(st), expr2.getType(st))
    }
  }

  object NewPair {
    def apply(expr1: Parsley[Expr], expr2: Parsley[Expr]): Parsley[NewPair] =
      pos <**> (expr1, expr2).zipped(NewPair(_, _) _)
  }

  case class Call(ident: Ident, argList: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable): Boolean = {
      println("Checking Call")
      val query = st.locateST(ident.name)
      if (query.isEmpty) {
        println("Error: " + ident + " is not defined")
        return false
      }
      val queryType = query.get.lookup(ident.name).get._2
      queryType match {
        case Func(_, _,_ ,_) =>
          val childFunctions = st.childFunctions
          for (child <- childFunctions) {
            val findChild = child.find(_._1 == ident.name)
            if (findChild.isEmpty) {
              println("Error: " + ident + " is not defined")
              return false
            }
            if (findChild.isDefined) {
              val funcST = findChild.get._2
              val dict = funcST.dictToList(funcST.dictionary)
              println("dict: " + dict)
              println("Found child function of args num: " + dict.length)
              println("Given args:" + argList.length)
              if (dict.length != argList.length) {
                println("Error: " + ident + " has wrong number of arguments")
                return false
              }
              for (i <- argList.indices) {
                if (!argList(i).check(st)) {
                  println("Error: " + ident + " has wrong type of arguments")
                  return false
                }
                val argType = argList(i).getType(st)
                val paramType = dict(i)._1
                println("argType: " + argType)
                println("paramType: " + paramType)
                if (argType != paramType) {
                  println("Error: " + ident + " has wrong type of arguments")
                  return false
                }
              }
            }
          }
          println("Child functions: " + childFunctions)
          return true
        case _ =>
          println("Error: " + ident + " is not a function")
          return false
      }
      true
    }

    override def getType(st: SymbolTable): TypeST = IntST()
  } // args-list only appears once

  object Call {
    def apply(ident: Parsley[Ident], argList: Parsley[List[Expr]]): Parsley[Call] =
      pos <**> (ident, argList).zipped(Call(_, _) _)
  }

  // <TYPE>
  sealed trait Type extends ASTNode {
    def check(st: SymbolTable): Boolean

    def getType(st: SymbolTable): TypeST
  }

  // <BASE-TYPE>
  sealed trait BaseType extends Type

  case class IntType()(val pos: (Int, Int)) extends BaseType {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = IntST()
  }

  object IntType extends ParserBridgePos0[IntType]

  case class BoolType()(val pos: (Int, Int)) extends BaseType {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = BoolST()
  }

  object BoolType extends ParserBridgePos0[BoolType]

  case class CharType()(val pos: (Int, Int)) extends BaseType {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = CharST()
  }

  object CharType extends ParserBridgePos0[CharType]

  case class StringType()(val pos: (Int, Int)) extends BaseType {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = StringST()
  }

  object StringType extends ParserBridgePos0[StringType]

  // <ARRAY-TYPE>
  case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = ArrayST(t.getType(st))
  }

  object ArrayType extends ParserBridgePos1[Type, ArrayType]

  // <PAIR-TYPE>
  case class PairType(t1: Type, t2: Type)(val pos: (Int, Int)) extends Type {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = PairST(t1.getType(st), t2.getType(st))
  }

  object PairType {
    def apply(t1: Parsley[Type], t2: Parsley[Type]): Parsley[PairType] =
      pos <**> (t1, t2).zipped(PairType(_, _) _)
  }

  case class NestedPairType()(val pos: (Int, Int)) extends Type {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = PairST(AnyST(), AnyST())
  }

  object NestedPairType extends ParserBridgePos0[NestedPairType]

  // <EXPR>
  sealed trait Expr extends Rvalue {
    def check(st: SymbolTable): Boolean

    def getType(st: SymbolTable): TypeST
  }

  case class IntLiter(value: Int)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable): Boolean = true

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object IntLiter {
    def apply(value: Parsley[Int]): Parsley[IntLiter] =
      pos <**> value.map(IntLiter(_) _)
  }

  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable): Boolean = true

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object BoolLiter {
    def apply(value: Parsley[Boolean]): Parsley[BoolLiter] =
      pos <**> value.map(BoolLiter(_) _)
  }

  case class CharLiter(value: Char)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable): Boolean = true

    def getType(st: SymbolTable): TypeST = CharST()
  }

  object CharLiter {
    def apply(value: Parsley[Char]): Parsley[CharLiter] =
      pos <**> value.map(CharLiter(_) _)
  }

  case class StrLiter(value: String)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable): Boolean = true

    def getType(st: SymbolTable): TypeST = StringST()
  }

  object StrLiter {
    def apply(value: Parsley[String]): Parsley[StrLiter] =
      pos <**> value.map(StrLiter(_) _)
  }

  case class PairLiter()(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable): Boolean = true

    def getType(st: SymbolTable): TypeST = PairST(AnyST(), AnyST())
  }

  object PairLiter extends ParserBridgePos0[PairLiter]

  // <UNARY-OP>
  sealed trait UnaryOp extends Expr

  case class Not(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Not")
      expr.getType(st) == BoolST() && expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object Not extends ParserBridgePos1[Expr, Not]

  case class Neg(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Neg")
      expr.getType(st) == IntST() && expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Neg extends ParserBridgePos1[Expr, Neg]


  case class Len(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Len")
      expr.getType(st) match {
        case ArrayST(_) => expr.check(st)
        case _ => false
      }
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Len extends ParserBridgePos1[Expr, Len]


  case class Ord(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Ord")
      expr.getType(st) == CharST() && expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Ord extends ParserBridgePos1[Expr, Ord]


  case class Chr(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Chr")
      expr.getType(st) == IntST() && expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = CharST()
  }

  object Chr extends ParserBridgePos1[Expr, Chr]


  // <BINARY-OP>
  sealed trait BinaryOp extends Expr

  case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Mul")
      expr1.getType(st) == IntST() && expr2.getType(st) == IntST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Mul extends ParserBridgePos2[Expr, Expr, Mul]

  case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Div")
      expr1.getType(st) == IntST() && expr2.getType(st) == IntST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Div extends ParserBridgePos2[Expr, Expr, Div]

  case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Mod")
      expr1.getType(st) == IntST() && expr2.getType(st) == IntST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Mod extends ParserBridgePos2[Expr, Expr, Mod]

  case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Add")
      expr1.getType(st) == IntST() && expr2.getType(st) == IntST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Add extends ParserBridgePos2[Expr, Expr, Add]

  case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Sub")
      expr1.getType(st) == IntST() && expr2.getType(st) == IntST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Sub extends ParserBridgePos2[Expr, Expr, Sub]

  case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking GT")
      (expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST()) && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object GT extends ParserBridgePos2[Expr, Expr, GT]

  case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking GTE")
      (expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST()) && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object GTE extends ParserBridgePos2[Expr, Expr, GTE]

  case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking LT")
      (expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST()) && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object LT extends ParserBridgePos2[Expr, Expr, LT]

  case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking LTE")
      (expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST()) && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object LTE extends ParserBridgePos2[Expr, Expr, LTE]

  case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking EQ")
      expr1.getType(st) == expr2.getType(st) && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object EQ extends ParserBridgePos2[Expr, Expr, EQ]

  case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking NEQ")
      expr1.getType(st) == expr2.getType(st) && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object NEQ extends ParserBridgePos2[Expr, Expr, NEQ]

  case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking And")
      expr1.getType(st) == BoolST() && expr2.getType(st) == BoolST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object And extends ParserBridgePos2[Expr, Expr, And]

  case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable): Boolean = {
      println("Checking Or")
      expr1.getType(st) == BoolST() && expr2.getType(st) == BoolST() && expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object Or extends ParserBridgePos2[Expr, Expr, Or]
}