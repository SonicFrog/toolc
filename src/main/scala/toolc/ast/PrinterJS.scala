package toolc
package ast

import Trees._
import analyzer.Symbols._

object PrinterJS {
  
  def apply(t: Tree, methodScope: Option[Set[String]]): String = {
    t match {
      case Program(main, classes) => List(classes.map(this(_, None)).mkString("\n"), this(main, None)).mkString("\n")

      case dcl : MainObject => dcl.stats.map(this(_, None)).mkString("\n")

      case dcl : ClassDecl => 
        val clName = this(dcl.id, None)
        dcl.parent.flatMap(parent => Some(clName + ".prototype = Object.create(" + this(parent, None) + ".prototype);\n")).getOrElse("") +
        "function " + clName + "() {" +
        dcl.vars.map(vari => this(vari.id, Some(Set())) + " = null;\n").mkString + "}\n" +
        dcl.methods.map(meth => clName + ".prototype." + this(meth, None)).mkString

      case dcl : MethodDecl => {
        val methScope = Some((dcl.vars.map (v => v.id.value) toSet) ++: (dcl.args.map (v => v.id.value) toSet))
        this(dcl.id, None) + " = function(" + dcl.args.map(this(_, None)).mkString(", ") +") { \n" +
        dcl.vars.map(this(_, None)).mkString("\n") + dcl.stats.map(this(_, methScope)).mkString("\n") +
        "return " + this(dcl.retExpr, methScope) + ";\n}\n"
      }
      
      case dcl : VarDecl => "var " + this(dcl.id, methodScope) + ";"
      case Formal(tpe, id) => this(id, methodScope)

      case IntArrayType() | IntType() | BooleanType() | StringType() => "" //nothing, types doesn't matter in JS.

      case Block(stats) => " {\n" + stats.map(this(_, methodScope)).mkString("\n") + "}"
      case If(expr, thn, els) => "if (" + this(expr, methodScope) + ")" + this(thn, methodScope) +
        els.flatMap(x => Some(" else " + this(x, methodScope))).getOrElse("")
      case While(expr, stats) => "while(" + this(expr, methodScope) + ")" + this(stats, methodScope)
      case Println(expr) => "console.log("+ this(expr, methodScope) + ");"
      case Assign(id, expr) => this(id, methodScope) + " = " + this(expr, methodScope) + ";"
      case ArrayAssign(id, index, expr) =>
        this(id, methodScope) + "[" + this(index, methodScope) + "] = " + this(expr, methodScope) +";"

      case And(lhs, rhs) => "(" + this(lhs, methodScope) + " && " + this(rhs, methodScope) + ")"
      case Or(lhs, rhs) => "(" + this(lhs, methodScope) + " || " + this(rhs, methodScope) + ")"
      case Plus(lhs, rhs) => "(" + this(lhs, methodScope) + " + " + this(rhs, methodScope) + ")"
      case Minus(lhs, rhs) => "(" + this(lhs, methodScope) + " - " + this(rhs, methodScope) + ")"
      case Times(lhs, rhs) => "(" + this(lhs, methodScope) + " * " + this(rhs, methodScope) + ")"
      case Div(lhs, rhs) => "(" + this(lhs, methodScope) + " / " + this(rhs, methodScope) + ")"
      case LessThan(lhs, rhs) => "(" + this(lhs, methodScope) + " < " + this(rhs, methodScope) + ")"
      case Equals(lhs, rhs) => "(" + this(lhs, methodScope) + " == " + this(rhs, methodScope) + ")"
      case ArrayRead(arr, index) => this(arr, methodScope) + "[" + this(index, methodScope) + "]"
      case ArrayLength(array) => this(array, methodScope) + ".length"

      case MethodCall(obj, meth, args) => this(obj, methodScope) + "." + this(meth, None) +"(" +
        args.map(x => this(x, methodScope)).mkString(", ") + ")"

      case IntLit(value) => value.toString
      case StringLit(value) => '"' + value + '"'

      case True() => "true"
      case False() => "false"
      case id : Identifier => methodScope match {
        case None => id.value
        case Some(ms) => ms.contains(id.value) match {
          case true => id.value
          case false => "this." + id.value
        }
      }
      case ths : This => "this"
      case NewIntArray(size) => "new Array("+ this(size, methodScope) +")"
      case New(tpe) => "new " + this(tpe, None) + "()"
      case Not(expr) => "!" + this(expr, methodScope)
    }
  }
}
