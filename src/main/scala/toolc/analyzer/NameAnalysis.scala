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
    def collectVariables(cls : ClassDecl) : List[VarDecl] = {
      def inner(sym : Map[String, VariableSymbol], vars : List[VarDecl], symbolized : List[VarDecl]) :
          List[VarDecl] = {
        vars match {
          case Nil => symbolized
          case x :: xs => sym.get(x.id.value) match {
            case None =>
              val ns =  new VariableSymbol(x.id.value)
              x.setSymbol(ns)
              val done = symbolized :+ x
              inner(sym.updated(x.id.value, ns), xs, done)
            case Some(ms) =>
              fatal(ms.name + " declared twice in class " + cls.id.value)
          }
        }
      }
      inner(Map(), cls.vars, List())
    }

    /**
      * Collects variables declared in the current method and checks if
      * they are defined twice in the current scope
      **/
    def collectMethodVariables(meth : MethodDecl, ms : MethodSymbol) : List[VarDecl] = {
      def inner(map : Map[String, VariableSymbol], vars : List[VarDecl], symbolized : List[VarDecl]) :
          List[VarDecl] = {
        vars match {
          case Nil => symbolized
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new VariableSymbol(x.id.value)
              x.setSymbol(ns)
              val done = symbolized :+ x
              inner(map.updated(x.id.value, ns), xs, done)
            case Some(vs) =>
              fatal(vs.name + " already declared in method " + ms.name)
          }
        }
      }
      inner(Map(), meth.vars, List())
    }

    def collectMethods(cls : ClassDecl, cs : ClassSymbol) : List[MethodDecl] = {
      def inner(map : Map[String, MethodSymbol], left : List[MethodDecl], symbolized : List[MethodDecl]) :
          List[MethodDecl] = left match {
        case Nil => symbolized
        case x :: xs => map.get(x.id.value) match {
          case None =>
            val ns = new MethodSymbol(x.id.value, cs)
            x.setSymbol(ns)
            val done = symbolized :+ x
            inner(map.updated(x.id.value, ns), xs, done)
          case Some(ms) =>
            fatal(ms.name + " declared twice for class " + cls.id.value)
        }
      }
      inner(Map(), cls.methods, List())
    }

    def collectClasses(prg : Program) : List[ClassDecl] = {
      def inner(map : Map[String, ClassSymbol], cls : List[ClassDecl], symbolized: List[ClassDecl]) : List[ClassDecl] = {
        cls match {
          case Nil => symbolized
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new ClassSymbol(x.id.value)
              x.setSymbol(ns)
              val done = symbolized :+ x
              inner(map.updated(x.id.value, ns), xs, done)
            case Some(cs) =>
              fatal(cs.name + " already declared")
          }
        }
      }
      inner(Map(), prg.classes, List())
    }

    // This is a suggestion:
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    val classes = collectClasses(prog)
    val methods = classes map ( cldcl => collectMethods(cldcl, cldcl.getSymbol))
    val methodsUpdated = for (mdlist <- methods) yield {
    	mdlist map { mdcl =>
    	  val varList = collectMethodVariables(mdcl, mdcl.getSymbol)
    	  new MethodDecl(mdcl.retType , mdcl.id, mdcl.args, varList, mdcl.stats, mdcl.retExpr)
    	}
    }
    val classVar = classes map (cldcl => collectVariables(cldcl))
    val classesWithVarSymb = classes zip (methodsUpdated zip (classVar))
    
    val classesWithAllSymb = classesWithVarSymb map {case (classe, (methods, classVar)) => new ClassDecl(classe.id, classe.parent, classVar, methods)}
    val prgm = new Program(prog.main, classesWithAllSymb)


    // Make sure you check for all constraints
    prgm
  }
}
