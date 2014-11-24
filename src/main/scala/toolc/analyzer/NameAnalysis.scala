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
              x.setSymbol(ns)
              inner(sym.updated(x.id.value, ns), xs)
            case Some(ms) =>
              fatal(ms.name + " declared twice in class " + cls.id.value)
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
              x.setSymbol(ns)
              inner(map.updated(x.id.value, ns), xs)
            case Some(vs) =>
              fatal(vs.name + " already declared in method " + ms.name)
          }
        }
      }
      inner(Map(), meth.vars)
    }

    def collectMethods(cls : ClassDecl, cs : ClassSymbol) : Map[String, MethodSymbol] = {
      def inner(map : Map[String, MethodSymbol], left : List[MethodDecl]) :
          Map[String, MethodSymbol] = left match {
        case Nil => map
        case x :: xs => map.get(x.id.value) match {
          case None =>
            val ns = new MethodSymbol(x.id.value, cs)
            x.setSymbol(ns)
            inner(map.updated(x.id.value, ns), xs)
          case Some(ms) =>
            fatal(ms.name + " declared twice for class " + cls.id.value)
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
              x.setSymbol(ns)
              inner(map.updated(x.id.value, ns), xs)
            case Some(cs) =>
              fatal(cs.name + " already declared")
          }
        }
      }
      inner(Map(), prg.classes)
    }

    // This is a suggestion:
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    prog.main.setSymbol(new ClassSymbol(prog.main.id.value))
    collectClasses(prog)
    prog.classes map ( cldcl => collectMethods(cldcl, cldcl.getSymbol))
    for (classe <- prog.classes) {
    	classe.methods map { mdcl =>
    	  collectMethodVariables(mdcl, mdcl.getSymbol)
    	}
    }
    val classVar = prog.classes map ( cldcl => collectVariables(cldcl))
    
    //prog.main.stats map 


    // Make sure you check for all constraints
    prog
  }
}
