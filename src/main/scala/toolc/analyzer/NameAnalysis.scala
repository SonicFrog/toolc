package toolc
package analyzer

import toolc.utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._


    /**
      * Collects class variables and checks if any is define twice
      **/
    def collectVariables(cls : ClassDecl) : Map[String, VariableSymbol] = {
      def inner(sym : Map[String, VariableSymbol], vars : List[VarDecl]) :
          Map[String, VariableSymbol] = {
        vars match {
          case Nil => sym
          case x :: xs => sym.get(x.id.value) match {
            case None =>
              val ns =  new VariableSymbol(x.id.value)
              x.id.setSymbol(ns)
              x.setSymbol(ns)
              inner(sym.updated(x.id.value, ns), xs)
            case Some(ms) =>
              fatal(ms.name + " declared twice in class " + cls.id.value, x)
          }
        }
      }
      inner(Map(), cls.vars)
    }

    /**
      * Collects variables declared in the current method and checks if
      * they are defined twice in the current scope
      **/
    def collectMethodVariables(meth : MethodDecl, ms : MethodSymbol) : Map[String, VariableSymbol] = {
      def inner(map : Map[String, VariableSymbol], vars : List[VarDecl]) :
          Map[String, VariableSymbol] = {
        vars match {
          case Nil => map
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new VariableSymbol(x.id.value)
              x.id.setSymbol(ns)
              x.setSymbol(ns)
              inner(map.updated(x.id.value, ns), xs)
            case Some(vs) =>
              fatal(vs.name + " already declared in method " + ms.name, x)
          }
        }
      }
      inner(Map(), meth.vars)
    }
    
    /**
      * Collects variables declared in the current method and checks if
      * they are defined twice in the current scope
      **/
    def collectMethodParam(meth : MethodDecl, ms : MethodSymbol) : Map[String, VariableSymbol] = {
      def inner(map : Map[String, VariableSymbol], args : List[Formal]) :
          Map[String, VariableSymbol] = {
        args match {
          case Nil => map
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new VariableSymbol(x.id.value)
              x.id.setSymbol(ns)
              x.setSymbol(ns)
              inner(map.updated(x.id.value, ns), xs)
            case Some(vs) =>
              fatal(vs.name + " duplicated argument name with " + ms.name, x)
          }
        }
      }
      inner(Map(), meth.args)
    }

    def collectMethods(cls : ClassDecl, cs : ClassSymbol) : Map[String, MethodSymbol] = {
      def inner(map : Map[String, MethodSymbol], left : List[MethodDecl]) :
          Map[String, MethodSymbol] = left match {
        case Nil => map
        case x :: xs => map.get(x.id.value) match {
          case None =>
            val ns = new MethodSymbol(x.id.value, cs)
            x.id.setSymbol(ns)
            x.setSymbol(ns)
            inner(map.updated(x.id.value, ns), xs)
          case Some(ms) =>
            fatal(ms.name + " declared twice for class " + cls.id.value, x)
        }
      }
      inner(Map(), cls.methods)
    }

    def collectClasses(prg : Program) : Map[String, ClassSymbol] = {
      def inner(map : Map[String, ClassSymbol], cls : List[ClassDecl]) : Map[String, ClassSymbol] = {
        cls match {
          case Nil => map
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new ClassSymbol(x.id.value)
              x.id.setSymbol(ns)
              x.setSymbol(ns)
              inner(map.updated(x.id.value, ns), xs)
            case Some(cs) =>
              fatal(cs.name + " already declared", x)
          }
        }
      }
      inner(Map(), prg.classes)
    }
    
    // This is a suggestion:
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    val globalScope = new GlobalScope
    
    val mainSymb = new ClassSymbol(prog.main.id.value)
    prog.main.id.setSymbol(mainSymb)
    globalScope.mainClass  = mainSymb
    
    val classes = collectClasses(prog)
    globalScope.classes = classes
   
    val methods = prog.classes flatMap ( cldcl => {
      val map = collectMethods(cldcl, cldcl.getSymbol)
      cldcl.getSymbol.methods = map
      map values
      })
    val classVar = prog.classes flatMap ( cldcl => {
      val map = collectVariables(cldcl)
      cldcl.getSymbol.members = map
      cldcl.getSymbol.parent  = cldcl.parent.flatMap(x => Some(classes(x.value)))
      map values
      })
    
    val methodVar = (for (classe <- prog.classes) yield {
    	classe.methods flatMap { mdcl => {
    		val mapMeth = collectMethodVariables(mdcl, mdcl.getSymbol)
    		val mapParam = collectMethodParam(mdcl, mdcl.getSymbol) 
    		mdcl.getSymbol.params = mapParam
    		mdcl.getSymbol.members = mapMeth
    		mapMeth ++: mapParam values
    		}
    	}
    }) flatten
    
    
    val listSymClasses = (classes values) toList
    var allSymbols = (listSymClasses ::: methods ::: methodVar ::: classVar) toSet
    
    prog.main.stats foreach {
      handleStatTree(_, null)
    }
    
    prog.classes foreach (
      _.methods foreach (
          meth => meth.stats foreach (handleStatTree(_, meth.getSymbol))
          )
    )
    
    prog.classes foreach ( cldcl => {
      cldcl.vars foreach (vrdcl => attachSymbolToType(vrdcl.tpe))
      cldcl.methods foreach { meth => meth.vars foreach (vrdcl => attachSymbolToType(vrdcl.tpe))
      	attachSymbolToType(meth.retType)
      	handleExprTree(meth.retExpr, meth.getSymbol)
      	meth.args foreach (arg => attachSymbolToType(arg.tpe))
      }
      })
    
   def attachSymbolToType(tpe : TypeTree) {
      tpe match {
        case id : Identifier =>
        	val ns = globalScope.lookupClass(id.value).getOrElse(fatal(id.value + " undeclared", id))
        	id.setSymbol(ns)
        case _ => //ain't no symbols to attach
      }
    }
    
    
   def attachVarSymbol(id: Identifier, sym: MethodSymbol) {
      sym.lookupVar(id.value) match {
      	  case None => fatal(id.value + " has not been declared in " + sym.name, id)
      	  case Some(smbl) => {
      	    id.setSymbol(smbl)
      	    allSymbols = allSymbols - smbl
      	  }
      	}
    }
    
     def handleStatTree(st : StatTree, sym : MethodSymbol) : Unit = {
      st match {
        case Block(stats) => stats foreach ( stats => handleStatTree(stats, sym))
      	case If(expr, thn, els) => handleExprTree(expr, sym)
      		handleStatTree(thn, sym)
      		els match {
      			case None =>
      			case Some(st) => handleStatTree(st, sym)
      		}
      	case While(expr, stat) => handleExprTree(expr, sym)
      		handleStatTree(stat, sym)
      	case Println(expr) => handleExprTree(expr, sym)
      	case Assign(id, expr) => attachVarSymbol(id, sym)
      		handleExprTree(expr, sym)
      	case ArrayAssign(id,index,expr) => attachVarSymbol(id, sym)
      	handleExprTree(index, sym)
      	handleExprTree(expr, sym)
      }
    }
    
    def handleExprTree(et : ExprTree, sym : MethodSymbol) : Unit = {
      et match {
        case And(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case Or(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case Plus(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case Minus(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case Times(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case Div(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case LessThan(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case Equals(lhs, rhs) => handleExprTree(lhs, sym)
        		handleExprTree(rhs, sym)
        case ArrayRead(arr, index) =>  handleExprTree(arr, sym)
        		handleExprTree(index, sym)
        case ArrayLength(arr) => handleExprTree(arr, sym)
        case MethodCall(obj, meth, args) => handleExprTree(obj, sym)
          // we do nothing with the method id.
          args foreach (handleExprTree(_, sym))
          
          
        case id : Identifier => attachVarSymbol(id, sym)
        case ths : This => ths.setSymbol(sym.classSymbol)
        case NewIntArray(size) => handleExprTree(size, sym)
        case New(tpe) => tpe.setSymbol(globalScope.lookupClass(tpe.value).getOrElse(fatal(tpe.value + " not declared", tpe)))
        case Not(expr) => handleExprTree(expr, sym)
        case _ => //TODO
      }
    }



    // Make sure you check for all constraints
    prog
  }
}
