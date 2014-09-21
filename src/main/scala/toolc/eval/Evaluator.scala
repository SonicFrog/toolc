package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    // Initialize the context for the main method
    val ectx = new MainMethodContext

    // Evaluate each statement of the main method
    prog.main.stats.foreach(evalStatement(ectx, _))
  }

  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
    //Comme un block c'est juste une liste de statement on evalue chacun des statement individuellement
    case Block(stats) => stats.foreach(evalStatement(ectx, _)) 
    
    //Là on évalue seulement si la condition est vraie sinon on évalue els
    case If(expr, thn, els) => 
      if (evalExpr(ectx, expr).asBool) //Si la condition est vraie on execute le then
        evalStatement(ectx, thn) 
      else { //Sinon on regarde si on a un elseb
    	  els match {
          case None => //Là c'est le cas où on a pas de else
          case Some(stat) => evalStatement(ectx, stat) //Si on a quelque chose on avait un else qu'on évalue
        }
      }
      
    //Tant que l'expression évalue à vrai on evalu???e le statement associé
    case While(expr, stat) => while (evalExpr(ectx, expr).asBool) evalStatement(ectx, stat)
    
    //Là pas trop technique on print simplement la valeur de l'expression
    case Println(expr) => println(evalExpr(ectx, expr).asString)
    
    //Là j'ai un peu de mal à voir comment on récupére le nom de la variable en tant que String vu
    //on l'obtient comme un Identifier
    //J'ai fait ça mais en fait je sais pas si id.position est le nom de la variable identifiée par id
    case Assign(id, expr) => ectx.setVariable(id.position, evalExpr(ectx, expr))
    
    //Même chose ici vu qu'on a seulement un identifier pour l'array
    case ArrayAssign(id, index, expr) => {
      val array = ectx.getVariable(id.position).asArray
      array.setIndex(evalExpr(ectx, index).asInt, evalExpr(ectx, expr).asInt)
    }
      
    case _ =>
      fatal("unnexpected statement", stmt)
  }

  def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = e match {
    case IntLit(value)    => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True()           => BoolValue(true)
    case False()          => BoolValue(false)
    case And(lhs, rhs) => ???
    case Or(lhs, rhs)  => ???
    case Plus(lhs, rhs) => ???
    case Minus(lhs, rhs) => ???
    case Times(lhs, rhs) => ???
    case Div(lhs, rhs) => ???
    case LessThan(lhs, rhs) => ???
    case Not(expr) => ???
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) => ???
    case ArrayLength(arr) => ???
    case MethodCall(obj, meth, args) => ???
    case Identifier(name) => ???
    case New(tpe) => ???
    case This() => ???
    case NewIntArray(size) => ???
  }

  // Define the scope of evaluation, with methods to access/declare/set local variables(or arguments)
  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  // A Method context consists of the execution context within an object method.
  // getVariable can fallback to the fields of the current object
  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  // Special execution context for the main method, which is very limitted.
  class MainMethodContext extends EvaluationContext {
    def getVariable(name: String): Value          = fatal("The main method contains no variable and/or field")
    def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field")
    def declareVariable(name: String): Unit       = fatal("The main method contains no variable and/or field")
  }

  // Helper functions to query the current program
  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.flatMap(p => findClass(p.value).methods.find(_.id.value == name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  // Runtime evaluation values, with as* methods which act as typecasts for convenience.
  sealed abstract class Value {
    def asInt: Int            = fatal("Unnexpected value, found "+this+" expected Int")
    def asString: String      = fatal("Unnexpected value, found "+this+" expected String")
    def asBool: Boolean       = fatal("Unnexpected value, found "+this+" expected Boolean")
    def asObject: ObjectValue = fatal("Unnexpected value, found "+this+" expected Object")
    def asArray: ArrayValue   = fatal("Unnexpected value, found "+this+" expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal("Unknown field '"+name+"'")
      }
    }

    def getField(name: String) = {
      fields.get(name).flatten.getOrElse(fatal("Unknown field '"+name+"'"))
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
    def setIndex(i: Int, v: Int) {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(var v: String) extends Value {
    override def asString = v
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }
}

