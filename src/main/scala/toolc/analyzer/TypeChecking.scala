package toolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {
  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) => tcExpr(lhs, TBool); tcExpr(rhs, TBool)
        case Or(lhs, rhs) => tcExpr(lhs, TBool); tcExpr(rhs, TBool)

        case Plus(lhs, rhs) => {
          val t1 = tcExpr(lhs, TString, TInt)

          t1 match {
            case TString => tcExpr(rhs, TInt, TString); TString
            case TInt => tcExpr(rhs, TInt, TString)
            case _ => TError
          }
        }

        case Minus(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt)
        case Times(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt)
        case Div(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt)

        case LessThan(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt); TBool
        case Equals(lhs, rhs) => {
          val t1 = tcExpr(lhs, TAnyObject, TInt, TString, TBool)

          t1 match {
            case TObject(cs) => tcExpr(rhs, TAnyObject)
            case TInt => tcExpr(rhs, TInt)
            case TString => tcExpr(rhs, TString)
            case TBool => tcExpr(rhs, TBool)
            case TIntArray => tcExpr(rhs, TIntArray)
            case _ => TError
          }

          TBool
        }

        case ArrayRead(arr, index) => tcExpr(arr, TIntArray); tcExpr(index, TInt)
        case ArrayLength(arr) => tcExpr(arr, TIntArray); TInt

        case MethodCall(obj, meth, args) => {
          val t = tcExpr(obj, TAnyObject)

          t match {
            case TObject(cs) =>
              cs.lookupMethod(meth.value) match {
                case None => {
                  error("Method " + meth.value + " not found in type " + cs.name)
                  TError
                }
                case Some(method) => {
                  if (args.length != method.params.size) {
                    error("Can't call method " + method.name + " from " +
                      cs.name + " with " + args.length + " parameters")
                    TError
                  }
                  else method.getType
                }
              }
            case _ => t //The error is already signaled by recursive call to tcExpr
          }
        }

        case IntLit(value) => TInt
        case StringLit(value) => TString
        case True() | False() => TBool
        case id : Identifier => id.getType
        case ths: This => ths.getType
        case New(tpe) => tpe.getType
        case Not(expr) => tcExpr(expr, TBool)
        case NewIntArray(size) => tcExpr(size, TInt); TIntArray
      }



      // Check result and return a valid type in case of error
      if(expected.isEmpty) {
          tpe
      } else {
        if(!expected.exists(e => tpe.isSubTypeOf(e))) {
            error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
            expected.head
        } else {
            tpe
        }
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) => stats.foreach (tcStat(_))
        case If(cond, thn, els) => tcExpr(cond, TBool); tcStat(thn); els.foreach(tcStat(_))
        case While(cond, stat) => tcExpr(cond, TBool); tcStat(stat)
        case Println(expr) => tcExpr(expr, TString, TInt, TBool)
        case Assign(id, expr) => tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) =>
          tcExpr(id, TIntArray); tcExpr(index, TInt); tcExpr(expr, TInt)
      }
    }

    // Checking types of return stats
    prog.classes.foreach {
      cl => cl.methods foreach {
        meth => tcExpr(meth.retExpr, meth.getSymbol.getType)
      }
    }

    prog.main.stats.foreach(tcStat(_))

    prog.classes.foreach {
      x => x.methods.foreach {
        y => y.stats.foreach(tcStat(_))
      }
    }

    prog
  }
}
