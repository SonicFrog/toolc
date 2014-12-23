package toolc.ast

import Trees._

object PrinterJS {
  def apply(t: Tree): String = {
    t match {
      case Program(main, classes) => List(classes.map(this(_)).mkString("\n"), this(main)).mkString("\n")

      case dcl : MainObject => dcl.stats.map(this(_)).mkString("\n")

      case dcl : ClassDecl => 
        "var " + this(dcl.id) + " = {" +
        List(dcl.vars.map(x => this(x.id) + ":null"),
        dcl.methods.map(x => this(x))).flatten.mkString(", ") + "}"

      case dcl : MethodDecl =>
        this(dcl.id) + ":function(" + dcl.args.map(this(_)).mkString(", ") +") { \n" +
        dcl.vars.map(this(_)).mkString("\n") + dcl.stats.map(this(_)).mkString("\n") +
        "return " + this(dcl.retExpr) + ";\n}"

      case dcl : VarDecl => "var " + this(dcl.id) + ";"
      case Formal(tpe, id) => this(id)

      case IntArrayType() | IntType() | BooleanType() | StringType() => "" //nothing, types doesn't matter in JS.

      case Block(stats) => " {\n" + stats.map(this(_)).mkString("\n") + "}"
      case If(expr, thn, els) => "if (" + this(expr) + ")" + this(thn) +
        els.flatMap(x => Some(" else " + this(x))).getOrElse("")
      case While(expr, stats) => "while(" + this(expr) + ")" + this(stats)
      case Println(expr) => "console.log("+ this(expr) + ");"
      case Assign(id, expr) => this(id) + " = " + this(expr) + ";"
      case ArrayAssign(id, index, expr) =>
        this(id) + "[" + this(index) + "] = " + this(expr) +";"

      case And(lhs, rhs) => "(" + this(lhs) + " && " + this(rhs) + ")"
      case Or(lhs, rhs) => "(" + this(lhs) + " || " + this(rhs) + ")"
      case Plus(lhs, rhs) => "(" + this(lhs) + " + " + this(rhs) + ")"
      case Minus(lhs, rhs) => "(" + this(lhs) + " - " + this(rhs) + ")"
      case Times(lhs, rhs) => "(" + this(lhs) + " * " + this(rhs) + ")"
      case Div(lhs, rhs) => "(" + this(lhs) + " / " + this(rhs) + ")"
      case LessThan(lhs, rhs) => "(" + this(lhs) + " < " + this(rhs) + ")"
      case Equals(lhs, rhs) => "(" + this(lhs) + " == " + this(rhs) + ")"
      case ArrayRead(arr, index) => this(arr) + "[" + this(index) + "]"
      case ArrayLength(array) => this(array) + ".length"

      case MethodCall(obj, meth, args) => this(obj) + "." + this(meth) +"(" +
        args.map(x => this(x)).mkString(", ") + ")"

      case IntLit(value) => value.toString
      case StringLit(value) => '"' + value + '"'

      case True() => "true"
      case False() => "false"
      case id : Identifier => id.value
      case ths : This => "this"
      case NewIntArray(size) => "new Array("+ this(size) +")"
      case New(tpe) => this(tpe)
      case Not(expr) => "!" + this(expr)
    }
  }
}
