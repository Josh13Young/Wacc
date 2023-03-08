package wacc

import parsley.Parsley
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.position.pos
import wacc.frontend.STType._
import wacc.error.WaccSemanticErrorBuilder
import wacc.error.WaccSemanticErrorBuilder._
import wacc.frontend.SymbolTable

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
      val funSTs = new ListBuffer[SymbolTable]()
      // check parameters of all functions first, then check their body
      // add resulting symbol tables (for params) to list of functional symbol tables funSTs
      for (f <- functions) {
        f.checkParam(st) match {
          case Some(x) => funSTs += x
          case None => result = false
        }
      }
      if (!result) return false
      for (i <- functions.indices) {
        val funST = funSTs(i)
        val f = functions(i)
        if (!f.checkStat(funST)) {
          result = false
        }
      }
      result &= stat.forall(_.check(st))
      result
    }
  }

  object Program extends ParserBridgePos2[List[Func], List[Stat], Program]

  // <FUNC>

  case class Func(t: Type, ident: Ident, vars: List[Param], stat: List[Stat])(val pos: (Int, Int)) extends ASTNode {
    var symbolTable = new SymbolTable(None)

    def checkParam(st: SymbolTable)(implicit err: SemanticError): Option[SymbolTable] = {
      val query = st.lookup(ident.name + "()")
      if (query.isDefined) {
        WaccSemanticErrorBuilder(pos, "Function " + ident.name + " already defined")
        return None
      }
      val funST = new SymbolTable(Some(st))
      var checkFail = false
      for (v <- vars) {
        if (!v.check(funST)) {
          checkFail = true
        }
        funST.add(v.ident.name, v.t.getType(funST), v)
      }
      if (checkFail) return None
      st.add(ident.name + "()", t.getType(st), this)
      // main st add child function table of this function
      st.addChildFunc(ident.name, funST)
      Some(funST)
    }

    def checkStat(funST: SymbolTable)(implicit errors: SemanticError): Boolean = {
      // new function symbol table for function body
      val funStatST = new SymbolTable(Some(funST))
      val st = funStatST.parentTable.get
      funStatST.isFunctionBody = true
      funStatST.functionReturnType = Some(t.getType(st))
      st.addChildFunc(ident.name, funStatST)
      symbolTable = funStatST
      if (!stat.forall(s => s.check(funStatST)))
        return false
      stat.last match {
        case Return(_) =>
          true
        case Exit(_) =>
          true
        case ifStat@If(_, _, _) =>
          if (!ifStat.hasReturnOrExit) {
            errors.isSemantic = false
            WaccSemanticErrorBuilder(pos, "Function " + ident.name + " does not return or exit a value")
          }
          ifStat.hasReturnOrExit
        case beginStat@BeginStat(_) =>
          if (!beginStat.hasReturnOrExit) {
            errors.isSemantic = false
            WaccSemanticErrorBuilder(pos, "Function " + ident.name + " does not return or exit a value")
          }
          beginStat.hasReturnOrExit
        case _ =>
          errors.isSemantic = false
          WaccSemanticErrorBuilder(pos, "Function " + ident.name + " does not return or exit a value")
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
      val query = st.lookup(ident.name)
      if (query.isDefined) {
        WaccSemanticErrorBuilder(pos, "Variable " + ident.name + " already defined")
        return false
      }
      true
    }
  }

  object Param extends ParserBridgePos2[Type, Ident, Param]

  // <STAT>
  sealed trait Stat extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean
  }

  case class Skip()(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true
  }

  object Skip extends ParserBridgePos0[Skip]

  case class AssignNew(t: Type, ident: Ident, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val query = st.lookup(ident.name)
      if (query.isDefined) {
        IdentAlreadyDefinedError(pos, ident.name)
        return false
      }
      val rhsType = rvalue.getType(st)
      val lhsType = t.getType(st)
      val resType = typeCompare(lhsType, rhsType)
      if (!typeCheck(resType)) {
        TypeMismatchError(pos, lhsType.toString, rhsType.toString)
        return false
      }
      if (!rvalue.check(st)) {
        return false
      }
      st.add(ident.name, resType, rvalue)
      true
    }
  }

  object AssignNew extends ParserBridgePos3[Type, Ident, Rvalue, AssignNew]

  case class Assign(lvalue: Lvalue, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (isNestedPair(lvalue) && isNestedPair(rvalue)) {
        WaccSemanticErrorBuilder(pos, "Nested pair, unknown type, erasure")
        return false
      }
      if (!lvalue.check(st)) {
        return false
      }
      val identType = lvalue.getType(st)
      val rhsType = rvalue.getType(st)
      val resType = typeCompare(identType, rhsType)
      if (!typeCheck(resType)) {
        TypeMismatchError(pos, identType.toString, rhsType.toString)
        return false
      }
      if (!rvalue.check(st)) {
        return false
      }
      true
    }

    private def isNestedPair(pair: ASTNode): Boolean = {
      pair match {
        case FstElem(FstElem(_)) => true
        case FstElem(SndElem(_)) => true
        case SndElem(FstElem(_)) => true
        case SndElem(SndElem(_)) => true
        case _ =>
          false
      }
    }
  }

  object Assign extends ParserBridgePos2[Lvalue, Rvalue, Assign]

  case class Read(lvalue: Lvalue)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val t = lvalue.getType(st)
      t match {
        case IntST() => lvalue.check(st)
        case CharST() => lvalue.check(st)
        // although null is not int or char, catch this error in runtime rather than here
        case NullST() => lvalue.check(st)
        case _ =>
          StatError(pos, "Read", Set("int", "char"), t.toString)
          false
      }
    }
  }

  object Read extends ParserBridgePos1[Lvalue, Read]

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val t = expr.getType(st)
      t match {
        case ArrayST(_) => expr.check(st)
        case PairST(_, _) => expr.check(st)
        case _ =>
          StatError(pos, "Free", Set("array", "pair"), t.toString)
          false
      }
    }
  }

  object Free extends ParserBridgePos1[Expr, Free]

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!st.isFunctionBody) {
        WaccSemanticErrorBuilder(pos, "Return expression not in a function body")
        return false
      }
      val requiredType = st.functionReturnType
      if (requiredType.isEmpty) {
        WaccSemanticErrorBuilder(pos, "Return " + expr + " wrong return type")
        return false
      }
      if (!expr.check(st)) {
        return false
      }
      if (typeCompare(requiredType.get, expr.getType(st)) == VoidST()) {
        TypeMismatchError(pos, requiredType.get.toString, expr.getType(st).toString)
        return false
      }
      true
    }
  }

  object Return extends ParserBridgePos1[Expr, Return]

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val t = expr.getType(st)
      if (t != IntST()) {
        StatError(pos, "Exit", Set("int"), t.toString)
        return false
      }
      expr.check(st)
    }
  }

  object Exit extends ParserBridgePos1[Expr, Exit]

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      expr.check(st)
    }
  }

  object Print extends ParserBridgePos1[Expr, Print]

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      expr.check(st)
    }
  }

  object Println extends ParserBridgePos1[Expr, Println]

  // Stat can call another Stat, so we represent it as a list of Stat
  case class If(cond: Expr, trueStat: List[Stat], falseStat: List[Stat])(val pos: (Int, Int)) extends Stat {
    var hasReturnOrExit = false
    var trueSymbolTable = new SymbolTable(None)
    var falseSymbolTable = new SymbolTable(None)

    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val condType = cond.getType(st)
      if (condType != BoolST()) {
        CondBoolError(pos, condType.toString)
        return false
      }
      if (!cond.check(st)) {
        return false
      }
      val trueST = new SymbolTable(Option(st))
      var trueHasReturnOrExit = false
      trueST.isFunctionBody = st.isFunctionBody
      trueST.functionReturnType = st.functionReturnType
      if (!trueStat.forall(_.check(trueST))) {
        return false
      }
      trueStat.last match {
        case Return(_) => trueHasReturnOrExit = true
        case Exit(_) => trueHasReturnOrExit = true
        case lastIf@If(_, _, _) => trueHasReturnOrExit = lastIf.hasReturnOrExit
        case lastBegin@BeginStat(_) => trueHasReturnOrExit = lastBegin.hasReturnOrExit
        case _ =>
      }
      val falseST = new SymbolTable(Option(st))
      var falseHasReturnOrExit = false
      falseST.isFunctionBody = st.isFunctionBody
      falseST.functionReturnType = st.functionReturnType
      if (!falseStat.forall(_.check(falseST))) {
        return false
      }
      falseStat.last match {
        case Return(_) => falseHasReturnOrExit = true
        case Exit(_) => falseHasReturnOrExit = true
        case lastIf@If(_, _, _) => falseHasReturnOrExit = lastIf.hasReturnOrExit
        case lastBegin@BeginStat(_) => falseHasReturnOrExit = lastBegin.hasReturnOrExit
        case _ =>
      }
      hasReturnOrExit = trueHasReturnOrExit && falseHasReturnOrExit
      trueSymbolTable = trueST
      falseSymbolTable = falseST
      true
    }
  }

  object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]

  case class While(cond: Expr, stat: List[Stat])(val pos: (Int, Int)) extends Stat {
    var symbolTable = new SymbolTable(None)
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!cond.check(st)) {
        return false
      }
      if (cond.getType(st) != BoolST()) {
        CondBoolError(pos, cond.getType(st).toString)
        return false
      }
      val whileST = new SymbolTable(Option(st))
      whileST.isFunctionBody = st.isFunctionBody
      whileST.functionReturnType = st.functionReturnType
      if (!stat.forall(_.check(whileST))) {
        return false
      }
      symbolTable = whileST
      true
    }
  }

  object While extends ParserBridgePos2[Expr, List[Stat], While]

  case class BeginStat(stat: List[Stat])(val pos: (Int, Int)) extends Stat {
    var hasReturnOrExit = false

    var symbolTable = new SymbolTable(None)
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val beginST = new SymbolTable(Option(st))
      beginST.isFunctionBody = st.isFunctionBody
      beginST.functionReturnType = st.functionReturnType
      if (!stat.forall(_.check(beginST))) {
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
      symbolTable = beginST
      true
    }
  }

  object BeginStat extends ParserBridgePos1[List[Stat], BeginStat]

  // <LVALUE>
  sealed trait Lvalue extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST
  }

  // <IDENT>
  case class Ident(name: String)(val pos: (Int, Int)) extends Lvalue with Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val query = st.lookupAll(name)
      if (query.isEmpty) {
        WaccSemanticErrorBuilder(pos, "Variable " + name + " is not declared")
        false
      } else {
        true
      }
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      val query = st.lookupAll(name)
      if (query.isEmpty) {
        VoidST()
      } else {
        query.head._1
      }
    }
  }

  object Ident extends ParserBridgePos1[String, Ident]

  // <ARRAY-ELEM>
  case class ArrayElem(ident: Ident, exprList: List[Expr])(val pos: (Int, Int)) extends Lvalue with Expr {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      var result = true
      ident.check(st)
      for (expr <- exprList) {
        if (expr.getType(st) != IntST()) {
          WaccSemanticErrorBuilder(pos, "Array index must be of type int")
          result = false
        }
        expr.check(st)
      }
      result
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      var identType = ident.getType(st)
      val bracket: String = "[]" * (exprList.length - 1)
      for (_ <- 1 to exprList.length) {
        identType match {
          case ArrayST(t) =>
            identType = t
            bracket.dropRight(2)
          case _ =>
            identType = VoidST()
        }
      }
      if (identType == VoidST()) {
        WaccSemanticErrorBuilder(pos, "variable \"" + ident.name + bracket + "\" is not an array")
      }
      identType
    }
  }

  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]

  // <PAIR-ELEM>
  sealed trait PairElem extends Lvalue with Rvalue

  case class FstElem(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      lvalue.getType(st) match {
        case PairST(_, _) => lvalue.check(st)
        case _ => false
      }
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      lvalue.getType(st) match {
        case PairST(t1, _) => t1
        case _ => VoidST()
      }
    }
  }

  object FstElem extends ParserBridgePos1[Lvalue, FstElem]

  case class SndElem(lvalue: Lvalue)(val pos: (Int, Int)) extends PairElem {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      lvalue.getType(st) match {
        case PairST(_, _) => lvalue.check(st)
        case _ => false
      }
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      lvalue.getType(st) match {
        case PairST(_, t2) => t2
        case _ => VoidST()
      }
    }
  }

  object SndElem extends ParserBridgePos1[Lvalue, SndElem]

  // <RVALUE>
  sealed trait Rvalue extends ASTNode {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST
  }

  // <ARRAY-LITER>
  case class ArrayLiter(exprList: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    var arrayType: TypeST = _
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      var result = true
      if (exprList.nonEmpty) {
        val elemType = exprList.head.getType(st)
        for (expr <- exprList) {
          if (!expr.check(st)) {
            result = false
          }
          if (expr.getType(st) != elemType) {
            WaccSemanticErrorBuilder(pos, "Array element types mismatch.\nArray elements must be of the same type")
            result = false
          }
        }
      }
      result
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      if (exprList.isEmpty) {
        arrayType = NullST()
        ArrayST(NullST())
      } else {
        val exprType = exprList.head.getType(st)
        arrayType = exprType
        ArrayST(exprType)
      }
    }
  }

  object ArrayLiter extends ParserBridgePos1[List[Expr], ArrayLiter]

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      expr1.check(st) && expr2.check(st)
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      PairST(expr1.getType(st), expr2.getType(st))
    }
  }

  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]

  case class Call(ident: Ident, argList: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    var symbolTable: SymbolTable = _
    override def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val query = st.locateST(ident.name + "()")
      if (query.isEmpty) {
        WaccSemanticErrorBuilder(pos, ident.name + " is not a defined function")
        return false
      }
      // function found in symbol table, get function param symbol table (not the table for function body)
      val funcST = query.get.getChildFunc(ident.name)
      // the function param symbol table's dictionary should be all the params
      val dict = funcST.dictToList()
      if (dict.length != argList.length) {
        WaccSemanticErrorBuilder(pos, ident.name + " has wrong number of arguments.\nGiven: " + argList.length + " Expected: " + dict.length)
        return false
      }
      if (!argList.forall(_.check(st))) {
        return false
      }
      var result = true
      for (i <- dict.indices) {
        val argType = argList(i).getType(st)
        val paramType = dict(i)._1
        if (typeCompare(argType, paramType) == VoidST()) {
          WaccSemanticErrorBuilder(argList(i).pos, ident.name + " has wrong type of arguments.\nGiven: " + argType.toString + " Expected: " + paramType.toString)
          result = false
        }
      }
      symbolTable = funcST
      result
    }

    override def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = {
      val query = st.lookupAll(ident.name + "()")
      if (query.isEmpty) {
        WaccSemanticErrorBuilder(pos, "function " + ident.name + " is not defined")
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

  // <EXPR>
  sealed trait Expr extends Rvalue {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST
  }

  case class IntLiter(value: Int)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object IntLiter extends ParserBridgePos1[Int, IntLiter]

  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]

  case class CharLiter(value: Char)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = CharST()
  }

  object CharLiter extends ParserBridgePos1[Char, CharLiter]

  case class StrLiter(value: String)(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = StringST()
  }

  object StrLiter extends ParserBridgePos1[String, StrLiter]

  case class PairLiter()(val pos: (Int, Int)) extends Expr {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = true

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = PairST(NullST(), NullST())
  }

  object PairLiter extends ParserBridgePos0[PairLiter]

  // <UNARY-OP>
  sealed trait UnaryOp extends Expr

  case class Not(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr.check(st)) return false
      val exprType = expr.getType(st)
      if (exprType != BoolST()) {
        UnaryOperatorError(pos, "!", "bool", exprType.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object Not extends ParserBridgePos1[Expr, Not]

  case class Neg(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr.check(st)) return false
      val exprType = expr.getType(st)
      if (exprType != IntST()) {
        UnaryOperatorError(pos, "-", "int", exprType.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Neg extends ParserBridgePos1[Expr, Neg]


  case class Len(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr.check(st)) return false
      val exprType = expr.getType(st)
      exprType match {
        case ArrayST(_) => true
        case _ =>
          UnaryOperatorError(pos, "len", "array", exprType.toString)
          false
      }
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Len extends ParserBridgePos1[Expr, Len]


  case class Ord(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr.check(st)) return false
      val exprType = expr.getType(st)
      if (expr.getType(st) != CharST()) {
        UnaryOperatorError(pos, "ord", "char", exprType.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Ord extends ParserBridgePos1[Expr, Ord]


  case class Chr(expr: Expr)(val pos: (Int, Int)) extends UnaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr.check(st)) return false
      val exprType = expr.getType(st)
      if (exprType != IntST()) {
        UnaryOperatorError(pos, "chr", "int", exprType.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = CharST()
  }

  object Chr extends ParserBridgePos1[Expr, Chr]


  // <BINARY-OP>
  sealed trait BinaryOp extends Expr

  case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (t1 != IntST() || t2 != IntST()) {
        BinaryOperatorError(pos, "*", Set("int"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Mul extends ParserBridgePos2[Expr, Expr, Mul]

  case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (t1 != IntST() || t2 != IntST()) {
        BinaryOperatorError(pos, "/", Set("int"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Div extends ParserBridgePos2[Expr, Expr, Div]

  case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (t1 != IntST() || t2 != IntST()) {
        BinaryOperatorError(pos, "%", Set("int"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Mod extends ParserBridgePos2[Expr, Expr, Mod]

  case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (t1 != IntST() || t2 != IntST()) {
        BinaryOperatorError(pos, "+", Set("int"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Add extends ParserBridgePos2[Expr, Expr, Add]

  case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (t1 != IntST() || t2 != IntST()) {
        BinaryOperatorError(pos, "-", Set("int"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = IntST()
  }

  object Sub extends ParserBridgePos2[Expr, Expr, Sub]

  case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (!(t1 == IntST() && t2 == IntST() ||
        t1 == CharST() && t2 == CharST())) {
        BinaryOperatorError(pos, ">", Set("int", "char"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object GT extends ParserBridgePos2[Expr, Expr, GT]

  case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (!(t1 == IntST() && t2 == IntST() ||
        t1 == CharST() && t2 == CharST())) {
        BinaryOperatorError(pos, ">=", Set("int", "char"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object GTE extends ParserBridgePos2[Expr, Expr, GTE]

  case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (!(t1 == IntST() && t2 == IntST() ||
        t1 == CharST() && t2 == CharST())) {
        BinaryOperatorError(pos, "<", Set("int", "char"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object LT extends ParserBridgePos2[Expr, Expr, LT]

  case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (!(t1 == IntST() && t2 == IntST() ||
        t1 == CharST() && t2 == CharST())) {
        BinaryOperatorError(pos, "<=", Set("int", "char"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object LTE extends ParserBridgePos2[Expr, Expr, LTE]

  case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      if (typeCompare(expr1.getType(st), expr2.getType(st)) == VoidST()) {
        WaccSemanticErrorBuilder(pos, "Cannot compare types " + expr1.getType(st) + " and " + expr2.getType(st))
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object EQ extends ParserBridgePos2[Expr, Expr, EQ]

  case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      if (typeCompare(expr1.getType(st), expr2.getType(st)) == VoidST()) {
        WaccSemanticErrorBuilder(pos, "Cannot compare types " + expr1.getType(st) + " and " + expr2.getType(st))
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object NEQ extends ParserBridgePos2[Expr, Expr, NEQ]

  case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (!(t1 == BoolST() && t2 == BoolST())) {
        BinaryOperatorError(pos, "&&", Set("bool"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object And extends ParserBridgePos2[Expr, Expr, And]

  case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends BinaryOp {
    def check(st: SymbolTable)(implicit errors: SemanticError): Boolean = {
      if (!expr1.check(st) || !expr2.check(st)) return false
      val t1 = expr1.getType(st)
      val t2 = expr2.getType(st)
      if (!(t1 == BoolST() && t2 == BoolST())) {
        BinaryOperatorError(pos, "||", Set("bool"), t1.toString, t2.toString)
        return false
      }
      true
    }

    def getType(st: SymbolTable)(implicit errors: SemanticError): TypeST = BoolST()
  }

  object Or extends ParserBridgePos2[Expr, Expr, Or]
}