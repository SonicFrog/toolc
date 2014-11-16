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
    def collectVariables(cls : ClassDecl) : ClassSymbol = {
      val sym = new ClassSymbol(cls.id.value)
      cls.vars.foreach { x =>
        sym.members = sym.members.get(x.id.value) match {
          case Some(vs) => fatal(x.id.value + " already declared")
          case None => sym.members.updated(x.id.value, new VariableSymbol(x.id.value))
        }
      }
      sym
    }

    /**
      * Collects variables declared in the current method and checks if
      * they have been defined higher up in the scope.
      **/
    def collectMethodVariables(meth : MethodDecl, ms : MethodSymbol) : Map[String, VariableSymbol] = {
      def inner(map : Map[String, VariableSymbol], vars : List[VarDecl]) :
          Map[String, VariableSymbol] = {
        vars match {
          case Nil => map
          case x :: xs =>
            inner(map.updated(x.id.value, new VariableSymbol(x.id.value)), xs)
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
            inner(map.updated(x.id.value, new MethodSymbol(x.id.value, cs)), xs)
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
              inner(map.updated(x.id.value, new ClassSymbol(x.id.value)), xs)
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

    val classes = collectClasses(prog)

    prog.classes.zip(classes.map(_._2)).foreach {
      (cls : ClassDecl, clsSym : ClassSymbol) => {
        cls.setSymbol(collectMethods(cls, clsSym))
        cls.methods.foreach { y =>
          ???
        }
      }
    }

    // Make sure you check for all constraints
    prog
  }
}
