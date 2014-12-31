package toolc
package ast

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree
  case class MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol]
  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]
  case class MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol]
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]

  sealed trait TypeTree extends Tree with Typed
  case class IntType() extends TypeTree
  case class BooleanType() extends TypeTree
  case class StringType() extends TypeTree
  case class DoubleType() extends TypeTree
  case class ArrayType(tpe : TypeTree) extends TypeTree

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: ExprTree, index: ExprTree, expr: ExprTree) extends StatTree
  
  //IO specific method calls
  case class WriteLine(message : ExprTree) extends StatTree
  case class ShowPopup(message : ExprTree) extends StatTree

  sealed trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree
  case class DoubleLit(value: Double) extends ExprTree

  //IO specific method calls
  case class ReadString(message : ExprTree) extends ExprTree
  case class ReadDouble(message : ExprTree) extends ExprTree
  case class ReadInteger(message : ExprTree) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree
  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol] {
    override def getType = getSymbol.getType
  }
  case class This() extends ExprTree with Symbolic[ClassSymbol] {
    override def getType = getSymbol.getType
  }
  case class NewArray(size : ExprTree, tpe : TypeTree) extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree
}
