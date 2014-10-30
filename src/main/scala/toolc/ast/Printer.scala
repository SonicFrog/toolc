package toolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    t match {
      case Program(main, classes) => this(main) +
        classes.map(this(_)).mkString("\n")

      case MainObject(id, stats) =>
        "object " + this(id) + " : Unit = { " +
        stats.map(this(_)).mkString("\n") + "}"

      case ClassDecl(id, parent, vars, methods) =>
        "class " + this(id) +
        parent.flatMap(x => Some(" extends " + this(x))).getOrElse("") + "{" +
        vars.map(x => this(x)).mkString("\n") +
        methods.map(x => this(x)).mkString("\n") + "}"

      case MethodDecl(retType, id, args, vars, stats, retExpr) =>
        "def " + this(id) + "(" + args.map(this(_)).mkString(", ") +") : " + this(retType) + " = { \n" +
        vars.map(this(_)).mkString("\n") + stats.map(this(_)).mkString("\n") +
        "return " + this(retExpr) + ";\n}"

      case VarDecl(tpe, id) => "var " + this(id) + " : " + this(tpe) + ";"
      case Formal(tpe, id) => this(id) + " : " + this(tpe)

      case IntArrayType() => "Int[]"
      case IntType() => "Int"
      case BooleanType() => "Boolean"
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
      case MethodCall(obj, meth, args) => this(obj) + "." + this(meth) + "(" +
        args.map(x => this(x)).mkString(",") + ")"
      case IntLit(value) => value.toString
      case StringLit(value) => '"' + value + '"'

      case True() => "true"
      case False() => "false"
      case Identifier(value) => value
      case This() => "this"
      case NewIntArray(size) => "new Int["+ this(size) +"]"
      case New(tpe) => "new " + this(tpe)
      case Not(expr) => "!" + this(expr)
    }
  }
}
