package wacc

import parsley.Parsley
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.position.pos
import wacc.STType._
import wacc.error.WaccSemanticErrorBuilder
import wacc.error.WaccSemanticErrorBuilder._

import scala.collection.mutable.ListBuffer

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

  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D

    def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C]): Parsley[D] = pos <**> (x, y, z).zipped(apply(_, _, _) _)

    override def con(pos: (Int, Int)): (A, B, C) => D = apply(_, _, _)(pos)
  }

  // node that holds position information (line, column)
  sealed trait ASTNode {
    val pos: (Int, Int)
  }

  // <PROGRAM>
  // Since Stat; Stat is valid, we represent it as a list of Stat
  case class Program(functions: List[Func], stat: List[Stat])(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable)(implicit err: SemanticError): Boolean = {
      var result = true
      println("Checking program: ")
      val funSTs = new ListBuffer[SymbolTable]()
      for (f <- functions) {
        val funST = f.checkParam(st)
        if (funST == null) {
          return false
        }
        funSTs += funST
      }
      for (i <- functions.indices) {
        val funST = funSTs(i)
        val f = functions(i)
        if (!f.checkStat(funST)) {
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

  object Program extends ParserBridgePos2[List[Func], List[Stat], Program]

  // <FUNC>

  case class Func(t: Type, ident: Ident, vars: List[Param], stat: List[Stat])(val pos: (Int, Int)) extends ASTNode {
    def checkParam(st: SymbolTable)(implicit err: SemanticError): SymbolTable = {
      println("Checking function: " + ident.name)
      val query = st.lookup(ident.name + "()")
      if (query.isDefined) {
        println("Error: function " + ident.name + " already defined\n")
        return null
      }
      val funST = new SymbolTable(Some(st))
      for (v <- vars) {
        if (!v.check(funST)) {
          return null
        }
        funST.add(v.ident.name, v.getType(st), v)
      }
      st.add(ident.name + "()", t.getType(st), this)
      st.addChildFunc(ident.name, funST)
      println(funST)
      funST
    }

    def checkStat(funST: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val funStatST = new SymbolTable(Some(funST))
      val st = funStatST.parentTable.get
      funStatST.isFunctionBody = true
      funStatST.functionReturnType = Some(t.getType(st))
      for (s <- stat) {
        if (!s.check(funStatST)) {
          return false
        }
      }
      stat.last match {
        case Return(_) =>
          println("last statement is a return")
          true
        case Exit(_) =>
          println("last statement is an exit")
          true
        case If(_, _, _) =>
          println("last statement is an if")
          val ifStat = stat.last.asInstanceOf[If]
          if (!ifStat.hasReturnOrExit) {
            sys.exit(100)
          }
          true
        case BeginStat(_) =>
          println("last statement is a begin")
          val beginStat = stat.last.asInstanceOf[BeginStat]
          if (!beginStat.hasReturnOrExit) {
            sys.exit(100)
          }
          true
        case _ =>
          println("last statement is not a return or exit")
          // will change this - but this is a syntax error
          sys.exit(100)
          false
      }
    }
  }

  object Func {
    def apply(t: Parsley[Type], ident: Parsley[Ident], vars: Parsley[List[Param]], stat: Parsley[List[Stat]]): Parsley[Func] =
      pos <**> (t, ident, vars, stat).zipped(Func(_, _, _, _) _)
  }

  // <PARAM-LIST> omitted (only 1 occurrence)

  // <PARAM>
  case class Param(t: Type, ident: Ident)(val pos: (Int, Int)) extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
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

  object Param extends ParserBridgePos2[Type, Ident, Param]

  // <STAT>
  sealed trait Stat extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean
  }

  case class Skip()(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking skip")
      true
    }
  }

  object Skip extends ParserBridgePos0[Skip]

  case class AssignNew(t: Type, ident: Ident, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking assign new: " + ident.name)
      val query = st.lookup(ident.name)
      if (query.isDefined) {
        IdentAlreadyDefinedError(pos, ident.name)
        println("Error: " + ident.name + " already defined\n")
        return false
      }
      val rhsType = rvalue.getType(st)
      val lhsType = t.getType(st)
      println("LHS type:" + lhsType + " RHS type: " + rhsType)
      val resType = typeCompare(lhsType, rhsType)
      if (!typeCheck(resType)) {
        TypeMismatchError(pos, lhsType.toString, rhsType.toString)
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

  object AssignNew extends ParserBridgePos3[Type, Ident, Rvalue, AssignNew]

  case class Assign(lvalue: Lvalue, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking assign: " + lvalue)
      if (isNestedPair(lvalue) && isNestedPair(rvalue)) {
        return false
      }
      if (!lvalue.check(st)) {
        println("Error: " + lvalue + " LHS check failed\n")
        return false
      }
      val identType = lvalue.getType(st)
      val rhsType = rvalue.getType(st)
      println("LHS type:" + identType + " RHS type: " + rhsType)
      val resType = typeCompare(identType, rhsType)
      if (!typeCheck(resType)) {
        TypeMismatchError(pos, identType.toString, rhsType.toString)
        println("Error: " + lvalue + " type mismatch\n")
        return false
      }
      if (!rvalue.check(st)) {
        println("Error: " + lvalue + " RHS check failed\n")
        return false
      }
      true
    }

    private def isNestedPair(pair: ASTNode)(implicit errors: SemanticError): Boolean = {
      pair match {
        case FstElem(FstElem(_)) => true
        case FstElem(SndElem(_)) => true
        case SndElem(FstElem(_)) => true
        case SndElem(SndElem(_)) => true
        case _ =>
          WaccSemanticErrorBuilder(pos, "Nested pairs are not allowed")
          false
      }
    }
  }

  object Assign extends ParserBridgePos2[Lvalue, Rvalue, Assign]

  case class Read(lvalue: Lvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking read: " + lvalue)
      val t = lvalue.getType(st)
      t match {
        case IntST() => lvalue.check(st)
        case CharST() => lvalue.check(st)
        case _ =>
          WaccSemanticErrorBuilder(pos, "Read requires an int or char, given " + t)
          println("Error: " + lvalue + " is not an int or char\n")
          false
      }
    }
  }

  object Read extends ParserBridgePos1[Lvalue, Read]

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking free: " + expr)
      expr.getType(st) match {
        case ArrayST(_) => expr.check(st)
        case PairST(_, _) => expr.check(st)
        case _ =>
          WaccSemanticErrorBuilder(pos, "Free requires an array or pair, given " + expr.getType(st))
          println("Error: " + expr + " is not an array or pair\n")
          false
      }
    }
  }

  object Free extends ParserBridgePos1[Expr, Free]

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking return: " + expr)
      if (!st.isFunctionBody) {
        WaccSemanticErrorBuilder(pos, "Return " + expr + " not in function body (should not be here)")
        return false
      }
      val requiredType = st.functionReturnType
      if (requiredType.isEmpty) {
        WaccSemanticErrorBuilder(pos, "Return " + expr + " wrong return type")
        println("Error: " + expr + " not in function body (should not be here)\n")
        return false
      }
      if (typeCompare(requiredType.get, expr.getType(st)) == VoidST()) {
        TypeMismatchError(pos, requiredType.get.toString, expr.getType(st).toString)
        println("Error: Return " + expr + " type mismatch\n")
        return false
      }
      expr.check(st)
    }
  }

  object Return extends ParserBridgePos1[Expr, Return]

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking exit: " + expr)
      if (expr.getType(st) != IntST()) {
        WaccSemanticErrorBuilder(pos, "Exit requires an int, given " + expr.getType(st))
        return false
      }
      expr.check(st)
    }
  }

  object Exit extends ParserBridgePos1[Expr, Exit]

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking print: " + expr)
      expr.check(st)
    }
  }

  object Print extends ParserBridgePos1[Expr, Print]

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking println: " + expr)
      expr.check(st)
    }
  }

  object Println extends ParserBridgePos1[Expr, Println]

  // Stat can call another Stat, so we represent it as a list of Stat
  case class If(cond: Expr, trueStat: List[Stat], falseStat: List[Stat])(val pos: (Int, Int)) extends Stat {
    var hasReturnOrExit = false
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking if: " + cond + "...")
      val condType = cond.getType(st)
      if (condType != BoolST()) {
        CondBoolError(pos, condType.toString)
        println("Error: " + cond + " is not a boolean\n")
        return false
      }
      if (!cond.check(st)) {
        println("Error: " + cond + " check failed\n")
        return false
      }
      val trueST = new SymbolTable(Option(st))
      var trueHasReturnOrExit = false
      trueST.isFunctionBody = st.isFunctionBody
      trueST.functionReturnType = st.functionReturnType
      if (!trueStat.forall(_.check(trueST))) {
        println("Error: " + trueStat + " check failed\n")
        return false
      }
      trueStat.last match {
        case Return(_) => trueHasReturnOrExit = true
        case Exit(_) => trueHasReturnOrExit = true
        case If(_, _, _) =>
          val lastIf = trueStat.last.asInstanceOf[If]
          trueHasReturnOrExit = lastIf.hasReturnOrExit
        case _ =>
      }
      val falseST = new SymbolTable(Option(st))
      var falseHasReturnOrExit = false
      falseST.isFunctionBody = st.isFunctionBody
      falseST.functionReturnType = st.functionReturnType
      if (!falseStat.forall(_.check(falseST))) {
        println("Error: " + falseStat + " check failed\n")
        return false
      }
      falseStat.last match {
        case Return(_) => falseHasReturnOrExit = true
        case Exit(_) => falseHasReturnOrExit = true
        case If(_, _, _) =>
          val lastIf = falseStat.last.asInstanceOf[If]
          falseHasReturnOrExit = lastIf.hasReturnOrExit
        case BeginStat(_) =>
          val lastBegin = falseStat.last.asInstanceOf[BeginStat]
          falseHasReturnOrExit = lastBegin.hasReturnOrExit
        case _ =>
      }
      hasReturnOrExit = trueHasReturnOrExit && falseHasReturnOrExit
      true
    }
  }

  object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]

  case class While(cond: Expr, stat: List[Stat])(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking while: " + cond + "...")
      if (cond.getType(st) != BoolST()) {
        CondBoolError(pos, cond.getType(st).toString)
        println("Error: " + cond + " is not a boolean\n")
        return false
      }
      if (!cond.check(st)) {
        println("Error: " + cond + " check failed\n")
        return false
      }
      val whileST = new SymbolTable(Option(st))
      whileST.isFunctionBody = st.isFunctionBody
      whileST.functionReturnType = st.functionReturnType
      if (!stat.forall(_.check(whileST))) {
        println("Error: " + stat + " check failed\n")
        return false
      }
      true
    }
  }

  object While extends ParserBridgePos2[Expr, List[Stat], While]

  case class BeginStat(stat: List[Stat])(val pos: (Int, Int)) extends Stat {
    var hasReturnOrExit = false
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking begin: " + stat + "...")
      val beginST = new SymbolTable(Option(st))
      beginST.isFunctionBody = st.isFunctionBody
      beginST.functionReturnType = st.functionReturnType
      if (!stat.forall(_.check(beginST))) {
        println("Error: " + stat + " check failed\n")
        return false
      }
      stat.last match {
        case Return(_) => hasReturnOrExit = true
        case Exit(_) => hasReturnOrExit = true
        case If(_, _, _) =>
          val lastIf = stat.last.asInstanceOf[If]
          hasReturnOrExit = lastIf.hasReturnOrExit
        case BeginStat(_) =>
          val lastBegin = stat.last.asInstanceOf[BeginStat]
          hasReturnOrExit = lastBegin.hasReturnOrExit
        case _ =>
      }
      true
    }
  }

  object BeginStat extends ParserBridgePos1[List[Stat], BeginStat]

  // <LVALUE>
  sealed trait Lvalue extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean

    def getType(st: SymbolTable): TypeST
  }

  // <IDENT>
  case class Ident(name: String)(val pos: (Int, Int)) extends Lvalue with Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking ident: " + name)
      val query = st.lookupAll(name)
      if (query.isEmpty) {
        WaccSemanticErrorBuilder(pos, "Ident " + name + " is not declared")
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
        VoidST()
      } else {
        println("Ident " + name + " is declared (type)")
        query.head._1
      }
    }
  }

  object Ident extends ParserBridgePos1[String, Ident]

  // <ARRAY-ELEM>
  case class ArrayElem(ident: Ident, exprList: List[Expr])(val pos: (Int, Int)) extends Lvalue with Expr {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      var result = true
      println("Checking array elem: " + ident)
      ident.check(st)
      for (expr <- exprList) {
        if (expr.getType(st) != IntST()) {
          WaccSemanticErrorBuilder(pos, "Array index must be of type int")
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
      for (_ <- 1 to exprList.length) {
        identType match {
          case ArrayST(t) => identType = t
          case _ =>
            println("Error: " + ident + " is not an array")
            identType = VoidST()
        }
      }
      identType
    }
  }

  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]

  // <PAIR-ELEM>
  sealed trait PairElem extends Lvalue with Rvalue

  case class FstElem(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking fst of : " + lvalue)
      lvalue.getType(st) match {
        case PairST(_, _) =>
          lvalue.check(st)
        case _ =>
          WaccSemanticErrorBuilder(pos, lvalue + " is not a pair")
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

  object FstElem extends ParserBridgePos1[Lvalue, FstElem]

  case class SndElem(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking snd of : " + lvalue)
      lvalue.getType(st) match {
        case PairST(_, _) =>
          lvalue.check(st)
        case _ =>
          WaccSemanticErrorBuilder(pos, lvalue + " is not a pair")
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

  object SndElem extends ParserBridgePos1[Lvalue, SndElem]

  // <RVALUE>
  sealed trait Rvalue extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean

    def getType(st: SymbolTable): TypeST
  }

  // <ARRAY-LITER>
  case class ArrayLiter(exprList: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking array liter")
      var result = true
      if (exprList.isEmpty) {
        println("Success, empty array")
      } else {
        val elemType = exprList.head.getType(st)
        for (expr <- exprList) {
          if (expr.getType(st) != elemType) {
            WaccSemanticErrorBuilder(pos, "Array elements must be of the same type")
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

  object ArrayLiter extends ParserBridgePos1[List[Expr], ArrayLiter]

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking new pair")
      expr1.check(st) && expr2.check(st)
    }

    override def getType(st: SymbolTable): TypeST = {
      PairST(expr1.getType(st), expr2.getType(st))
    }
  }

  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]

  case class Call(ident: Ident, argList: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Call")
      val query = st.locateST(ident.name + "()")
      if (query.isEmpty) {
        println("Error: " + ident + " is not defined")
        return false
      }
      // function found in symbol table
      val funcST = query.get.getChildFunc(ident.name)
      println("ThisST: " + st)
      println("FuncST: " + funcST)
      val dict = funcST.dictToList()
      println("dict: " + dict)
      println("Found child function of args num: " + dict.length)
      println("Given args:" + argList.length)
      if (dict.length != argList.length) {
        println("Error: " + ident + " has wrong number of arguments")
        return false
      }
      for (i <- dict.indices) {
        if (!argList(i).check(st)) {
          return false
        }
        val argType = argList(i).getType(st)
        println("argType: " + argType)
        val paramType = dict(i)._1
        println("paramType: " + paramType)
        if (typeCompare(argType, paramType) == VoidST()) {
          println("Error: " + ident + " has wrong type of arguments")
          return false
        }
      }
      true
    }

    override def getType(st: SymbolTable): TypeST = {
      println("Getting type of function call")
      val query = st.lookupAll(ident.name + "()")
      if (query.isEmpty) {
        println("Error: " + ident + " is not defined")
        return VoidST()
      }
      query.get._1
    }
  } // args-list only appears once

  object Call extends ParserBridgePos2[Ident, List[Expr], Call]

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

  object PairType extends ParserBridgePos2[Type, Type, PairType]

  case class NestedPairType()(val pos: (Int, Int)) extends Type {
    override def check(st: SymbolTable): Boolean = true

    override def getType(st: SymbolTable): TypeST = PairST(AnyST(), AnyST())
  }

  object NestedPairType extends ParserBridgePos0[NestedPairType]

  // <EXPR>
  sealed trait Expr extends Rvalue {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean

    def getType(st: SymbolTable): TypeST
  }

  case class IntLiter(value: Int)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object IntLiter extends ParserBridgePos1[Int, IntLiter]

  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]

  case class CharLiter(value: Char)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable): TypeST = CharST()
  }

  object CharLiter extends ParserBridgePos1[Char, CharLiter]

  case class StrLiter(value: String)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable): TypeST = StringST()
  }

  object StrLiter extends ParserBridgePos1[String, StrLiter]

  case class PairLiter()(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable): TypeST = PairST(AnyST(), AnyST())
  }

  object PairLiter extends ParserBridgePos0[PairLiter]

  // <UNARY-OP>
  sealed trait UnaryOp extends Expr

  case class Not(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Not")
      if (expr.getType(st) != BoolST()) {
        UnaryOperatorError(pos, "!", "bool")
        return false
      }
      expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object Not extends ParserBridgePos1[Expr, Not]

  case class Neg(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Neg")
      if (expr.getType(st) != IntST()) {
        UnaryOperatorError(pos, "-", "int")
        return false
      }
      expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Neg extends ParserBridgePos1[Expr, Neg]


  case class Len(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Len")
      expr.getType(st) match {
        case ArrayST(_) => expr.check(st)
        case _ =>
          UnaryOperatorError(pos, "len", "array")
          false
      }
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Len extends ParserBridgePos1[Expr, Len]


  case class Ord(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Ord")
      if (expr.getType(st) != CharST()) {
        UnaryOperatorError(pos, "ord", "char")
        return false
      }
      expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Ord extends ParserBridgePos1[Expr, Ord]


  case class Chr(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Chr")
      if (expr.getType(st) != IntST()) {
        UnaryOperatorError(pos, "chr", "int")
        return false
      }
      expr.check(st)
    }

    def getType(st: SymbolTable): TypeST = CharST()
  }

  object Chr extends ParserBridgePos1[Expr, Chr]


  // <BINARY-OP>
  sealed trait BinaryOp extends Expr

  case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Mul")
      if (expr1.getType(st) != IntST() || expr2.getType(st) != IntST()) {
        BinaryOperatorError(pos, "*", Set("int"))
      return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Mul extends ParserBridgePos2[Expr, Expr, Mul]

  case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Div")
      if (expr1.getType(st) != IntST() || expr2.getType(st) != IntST()) {
        BinaryOperatorError(pos, "/", Set("int"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Div extends ParserBridgePos2[Expr, Expr, Div]

  case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Mod")
      if (expr1.getType(st) != IntST() || expr2.getType(st) != IntST()) {
        BinaryOperatorError(pos, "%", Set("int"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Mod extends ParserBridgePos2[Expr, Expr, Mod]

  case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Add")
      if (expr1.getType(st) != IntST() || expr2.getType(st) != IntST()) {
        BinaryOperatorError(pos, "+", Set("int"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Add extends ParserBridgePos2[Expr, Expr, Add]

  case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Sub")
      if (expr1.getType(st) != IntST() || expr2.getType(st) != IntST()) {
        BinaryOperatorError(pos, "-", Set("int"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = IntST()
  }

  object Sub extends ParserBridgePos2[Expr, Expr, Sub]

  case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking GT")
      if (!(expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST())) {
        BinaryOperatorError(pos, ">", Set("int", "char"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object GT extends ParserBridgePos2[Expr, Expr, GT]

  case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking GTE")
      if (!(expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST())) {
        BinaryOperatorError(pos, ">=", Set("int", "char"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object GTE extends ParserBridgePos2[Expr, Expr, GTE]

  case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking LT")
      if (!(expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST())) {
        BinaryOperatorError(pos, "<", Set("int", "char"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object LT extends ParserBridgePos2[Expr, Expr, LT]

  case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking LTE")
      if (!(expr1.getType(st) == IntST() && expr2.getType(st) == IntST() ||
        expr1.getType(st) == CharST() && expr2.getType(st) == CharST())) {
        BinaryOperatorError(pos, "<=", Set("int", "char"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object LTE extends ParserBridgePos2[Expr, Expr, LTE]

  case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking EQ")
      if (typeCompare(expr1.getType(st), expr2.getType(st)) == VoidST()) {
        WaccSemanticErrorBuilder(pos, "Cannot compare types " + expr1.getType(st) + " and " + expr2.getType(st))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object EQ extends ParserBridgePos2[Expr, Expr, EQ]

  case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking NEQ")
      if (typeCompare(expr1.getType(st), expr2.getType(st)) == VoidST()) {
        WaccSemanticErrorBuilder(pos, "Cannot compare types " + expr1.getType(st) + " and " + expr2.getType(st))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object NEQ extends ParserBridgePos2[Expr, Expr, NEQ]

  case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking And")
      if (!(expr1.getType(st) == BoolST() && expr2.getType(st) == BoolST())) {
        BinaryOperatorError(pos, "&&", Set("bool"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object And extends ParserBridgePos2[Expr, Expr, And]

  case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      println("Checking Or")
      if (!(expr1.getType(st) == BoolST() && expr2.getType(st) == BoolST())) {
        BinaryOperatorError(pos, "||", Set("bool"))
        return false
      }
      expr1.check(st) && expr2.check(st)
    }

    def getType(st: SymbolTable): TypeST = BoolST()
  }

  object Or extends ParserBridgePos2[Expr, Expr, Or]
}