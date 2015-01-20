package toolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {
  /**
    * Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages.
    */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBool); tcExpr(rhs, TBool)
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBool); tcExpr(rhs, TBool)

        case Plus(lhs, rhs) => {
          val t1 = tcExpr(lhs, TString, TInt, TDouble, TBool)

          t1 match {
            case TString =>
              tcExpr(rhs, TInt, TString, TDouble, TBool); TString
            case TInt => tcExpr(rhs, TInt, TString, TDouble)
            case TDouble => tcExpr(rhs, TInt, TString, TDouble) match {
              case TString => TString
              case _ => TDouble
            }
            case TBool => tcExpr(rhs, TString)
            case _ => TError
          }
        }

        case Minus(lhs, rhs) => {
          val t1 = tcExpr(lhs, TInt, TDouble)

          t1 match {
            case TInt => tcExpr(rhs, TInt, TDouble)
            case TDouble =>
              tcExpr(rhs, TInt, TDouble); TDouble
            case _ => TError
          }
        }

        case Times(lhs, rhs) => {
          val t1 = tcExpr(lhs, TInt, TDouble)

          t1 match {
            case TInt => tcExpr(rhs, TInt, TDouble)
            case TDouble =>
              tcExpr(rhs, TInt, TString, TDouble); TDouble
            case _ => TError
          }
        }

        case Div(lhs, rhs) => {
          val t1 = tcExpr(lhs, TInt, TDouble);
          t1 match {
            case TInt => tcExpr(rhs, TInt, TDouble)
            case _ => TDouble
          }
        }

        case LessThan(lhs, rhs) => {
          val t1 = tcExpr(lhs, TInt, TDouble)
          t1 match {
            case TInt => tcExpr(rhs, TInt); TBool
            case _ => tcExpr(rhs, TDouble); TBool
          }
        }

        case Equals(lhs, rhs) => {
          val t1 = tcExpr(lhs, TAnyObject, TInt, TString, TBool, TDouble, TGenericArray)

          t1 match {
            case TObject(cs) => tcExpr(rhs, TAnyObject)
            case TInt => tcExpr(rhs, TInt)
            case TDouble => tcExpr(rhs, TDouble)
            case TString => tcExpr(rhs, TString)
            case TBool => tcExpr(rhs, TBool)
            case arr: TArray => tcExpr(rhs, arr)
            case _ => TError
          }

          TBool
        }

        case ArrayRead(arr, index) =>
          tcExpr(index, TInt); tcExpr(arr, TGenericArray); arr.getType.asInstanceOf[TArray].innerType
        case ArrayLength(arr) =>
          tcExpr(arr, TGenericArray); TInt

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
                  } else {
                    meth.setSymbol(method)
                    val zipped = args zip method.argList
                    zipped foreach { tuple => tcExpr(tuple._1, tuple._2.getType) }
                    method.getType
                  }
                }
              }
            case _ => t //The error is already signaled by recursive call to tcExpr
          }
        }

        case IntLit(value) =>
          expr.setType(TInt); TInt
        case StringLit(value) => TString
        case DoubleLit(value) =>
          expr.setType(TDouble); TDouble
        case True() | False() => TBool
        case id: Identifier => id.getType
        case ths: This => ths.getType
        case New(tpe, args) => {
          (args zip expr.asInstanceOf[New].getSymbol.argList) foreach {
            tuple =>{
              tcExpr(tuple._1, tuple._2.getType)
            }
          }
          tpe.getType
        }
        case Not(expr) => tcExpr(expr, TBool)
        case NewArray(size, tpe) =>
          tcExpr(size, TInt); expr.getType

        case ReadString(msg) =>
          tcExpr(msg, TString); TString
        case ReadDouble(msg) =>
          tcExpr(msg, TString); TDouble
        case ReadInteger(msg) => tcExpr(msg, TString); TInt
      }

      expr.setType(tpe) //Assign type computed above to current expression

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else {
        if (!expected.exists(e => tpe.isSubTypeOf(e))) {
          error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
          expected.head
        } else {
          tpe
        }
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) => stats.foreach(tcStat(_))
        case If(cond, thn, els) =>
          tcExpr(cond, TBool); tcStat(thn); els.foreach(tcStat(_))
        case While(cond, stat) =>
          tcExpr(cond, TBool); tcStat(stat)
        case Assign(id, expr) => tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) =>
          tcExpr(id, TGenericArray); tcExpr(index, TInt); tcExpr(expr, id.getType.asInstanceOf[TArray].innerType)
        case Log(msg) => tcExpr(msg, TString)
        case WriteLine(msg) => tcExpr(msg, TString)
        case ShowPopup(msg) => tcExpr(msg, TString)
      }
    }

    prog.classes.foreach {
      cl =>
      cl.methods.foreach {
        meth =>
        meth.getSymbol.overridden match {
          case None =>
          case Some(over) => {
            if (over.getType != meth.getSymbol.getType) error(meth.id.value + " must return a " + over.getType, meth)
            over.argList.zip(meth.getSymbol.argList).foreach {
              x =>
              {
                if (x._1.getType != x._2.getType) error("Type mismatch in overriding method " + meth.id.value, meth)
              }
            }
          }
        }
      }
    }

    // Checking types of return stats
    prog.classes.foreach {
      cl =>
      cl.methods foreach {
        meth =>
        if (!tcExpr(meth.retExpr, meth.getSymbol.getType).isSubTypeOf(meth.getSymbol.getType))
          error("Type mismatch " + meth.id.value + " must return a " + meth.retType.getType, meth.retExpr)
      }
    }

    prog.main.stats.foreach(tcStat(_))

    prog.classes.foreach {
      x =>
      x.constructor.stats.foreach(tcStat(_))

      x.methods.foreach {
        y => y.stats.foreach(tcStat(_))
      }
    }

    prog
  }
}
