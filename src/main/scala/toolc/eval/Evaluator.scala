package toolc
package eval

import ast.Trees._
import utils._
import scala.reflect.internal.TreesStats

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    // Initialize the context for the main method
    val ectx = new MainMethodContext

    // Evaluate each statement of the main method
    prog.main.stats.foreach(evalStatement(ectx, _))
  }

  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
    //Since a block is only a list of statement we only have to evaluate each one 
    case Block(stats) => stats.foreach(evalStatement(ectx, _)) 
    
    case If(expr, thn, els) => 
      if (evalExpr(ectx, expr).asBool) //If the condition is True evaluate the statement
        evalStatement(ectx, thn) 
      else { //The if condition is False we need to check for an else
    	  els match {
          case None => //Here we have no else statement
          case Some(stat) => evalStatement(ectx, stat) //If we have an else statement we evaluate it
        }
      }
      
    //As long as expr evaluates to True we evaluate the associated statement
    case While(expr, stat) => while (evalExpr(ectx, expr).asBool) evalStatement(ectx, stat)
    
    //Là pas trop technique on print simplement la valeur de l'expression
    case Println(expr) => println(evalExpr(ectx, expr).asString)
    
    //See ArrayAssign
    case Assign(id, expr) => ectx.setVariable(id.value, evalExpr(ectx, expr))
    
    //My bad simply use id.value to get the name as a string ...
    case ArrayAssign(id, index, expr) => {
      val array = ectx.getVariable(id.value).asArray
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
    case And(lhs, rhs) => BoolValue(evalExpr(ectx, lhs).asBool && evalExpr(ectx, rhs).asBool)
    case Or(lhs, rhs)  => BoolValue(evalExpr(ectx, lhs).asBool || evalExpr(ectx, rhs).asBool)
    case Plus(lhs, rhs) => IntValue(evalExpr(ectx, lhs).asInt + evalExpr(ectx, rhs).asInt)
    case Minus(lhs, rhs) => IntValue(evalExpr(ectx, lhs).asInt - evalExpr(ectx, rhs).asInt)
    case Times(lhs, rhs) => IntValue(evalExpr(ectx, lhs).asInt * evalExpr(ectx, rhs).asInt)
    case Div(lhs, rhs) => IntValue(evalExpr(ectx, lhs).asInt / evalExpr(ectx, rhs).asInt)
    case LessThan(lhs, rhs) => BoolValue(evalExpr(ectx, lhs).asInt < evalExpr(ectx, rhs).asInt)
    case Not(expr) => BoolValue(!evalExpr(ectx, expr).asBool)
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) => IntValue(evalExpr(ectx, arr).asArray.getIndex(evalExpr(ectx, index).asInt))
    case ArrayLength(arr) => IntValue(evalExpr(ectx, arr).asArray.size) 
    
    case MethodCall(obj, meth, args) => {
      val objval : ObjectValue = evalExpr(ectx, obj).asObject //Evaluate the obj we run a method of
      val mctx : MethodContext = new MethodContext(objval) //Create a context for the method
      val method = findMethod(objval.cd, meth.value) //Get the methodDeclaration
      
      method.args.map(_.id).zip(args).foreach {
        case (id, expr) => { //Evaluate the value for the expression of each argument to the method call
        	mctx.declareVariable(id.value)
        	mctx.setVariable(id.value, evalExpr(ectx, expr))
        }
      }
      
      method.stats.foreach(evalStatement(mctx, _)) //Evaluate each statement that makes the method
      
      evalExpr(mctx, method.retExpr) //And finally evaluate and return the return value      
    }
    
    case Identifier(name) => ectx.getVariable(name)
    case New(tpe) => ObjectValue(findClass(tpe.value)) //Return a new ObjectValue of the correct type
    case This() => ectx match {
      case MethodContext(obj) => obj //TODO: MethodContext isn't a case class so we can't pattern match on it
      case _ => fatal("Can't use this outside of a method call")
    }
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

