package toolc.ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    t match {
      case Program(main, classes) => this(main) +
        classes.map(this(_)).mkString("\n")

      case dcl : MainObject =>
        "object " + this(dcl.id) + " {\n" +
          " def main() : Unit = { " +
        dcl.stats.map(this(_)).mkString("\n") + "}\n}"

      case dcl : ClassDecl =>
        "class " + this(dcl.id) +
        dcl.parent.flatMap(x => Some(" extends " + this(x))).getOrElse("") + "{" +
        dcl.vars.map(x => this(x)).mkString("\n") +
        dcl.methods.map(x => this(x)).mkString("\n") + "}"

      case dcl : MethodDecl =>
        "def " + this(dcl.id) + "(" + dcl.args.map(this(_)).mkString(", ") +") : " + this(dcl.retType) + " = { \n" +
        dcl.vars.map(this(_)).mkString("\n") + dcl.stats.map(this(_)).mkString("\n") +
        "return " + this(dcl.retExpr) + ";\n}"

      case dcl : VarDecl => "var " + this(dcl.id) + " : " + this(dcl.tpe) + ";"
      case Formal(tpe, id) => this(id) + " : " + this(tpe)

      case IntArrayType() => "Int[]"
      case IntType() => "Int"
      case BooleanType() => "Bool"
      case StringType() => "String"

      case Block(stats) => "{" + stats.map(this(_)).mkString("\n") + "}"
      case If(expr, thn, els) => "if (" + this(expr) + ")" + this(thn) +
        els.flatMap(x => Some(" else " + this(x))).getOrElse("")
      case While(expr, stats) => "while(" + this(expr) + ")" + this(stats)
      case Println(expr) => "println("+ this(expr) + ");"
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
      case MethodCall(obj, meth, args) => this(obj) + "." + meth.value + "#??(" +
        args.map(x => this(x)).mkString(",") + ")"
      case IntLit(value) => value.toString
      case StringLit(value) => '"' + value + '"'

      case True() => "true"
      case False() => "false"
      case id : Identifier => id.value + "#" + id.getSymbol.id  
      case ths : This => "this#" + ths.getSymbol.id 
      case NewIntArray(size) => "new Int["+ this(size) +"]"
      case New(tpe) => "new " + this(tpe) + "()"
      case Not(expr) => "!" + this(expr)
    }
  }
}
