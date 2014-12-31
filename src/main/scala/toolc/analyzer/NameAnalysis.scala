package toolc
package analyzer

import toolc.utils._
import ast.Trees._
import analyzer.Types._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val globalScope = new GlobalScope

    def fetchType(tpe: TypeTree): Types.Type = {
      println(tpe)
      tpe match {
        case _: IntType => TInt
        case _: BooleanType => TBool
        case _: StringType => TString
        case _: DoubleType => TDouble
        case ArrayType(innerType) => TArray(fetchType(innerType))
        case id: Identifier => globalScope.lookupClass(id.value) match {
          case None => TError
          case Some(cs) => new TObject(cs)
        }
      }
    }

    /**
     * Collects class variables and checks if any is define twice
     */
    def collectVariables(cls: ClassDecl): Map[String, VariableSymbol] = {
      def inner(sym: Map[String, VariableSymbol], vars: List[VarDecl]): Map[String, VariableSymbol] = {
        vars match {
          case Nil => sym
          case x :: xs => sym.get(x.id.value) match {
            case None =>
              val ns = new VariableSymbol(x.id.value)
              ns.setType(fetchType(x.tpe))
              ns.setPos(x)
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
     */
    def collectMethodVariables(meth: MethodDecl, ms: MethodSymbol): Map[String, VariableSymbol] = {
      def inner(map: Map[String, VariableSymbol], vars: List[VarDecl]): Map[String, VariableSymbol] = {
        vars match {
          case Nil => map
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new VariableSymbol(x.id.value)
              ns.setType(fetchType(x.tpe))
              ns.setPos(x)
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
     */
    def collectMethodParam(meth: MethodDecl, ms: MethodSymbol): (Map[String, VariableSymbol], List[VariableSymbol]) = {
      def inner(map: Map[String, VariableSymbol], args: List[Formal]): Map[String, VariableSymbol] = {
        args match {
          case Nil => map
          case x :: xs => map.get(x.id.value) match {
            case None =>
              val ns = new VariableSymbol(x.id.value)
              ns.setType(fetchType(x.tpe))
              ns.setPos(x)
              x.id.setSymbol(ns)
              x.setSymbol(ns)
              inner(map.updated(x.id.value, ns), xs)
            case Some(vs) =>
              fatal(vs.name + " duplicated argument name with " + ms.name, x)
          }
        }
      }
      val params = inner(Map(), meth.args)
      (params, meth.args map (u => u.getSymbol))
    }

    def collectMethods(cls: ClassDecl, cs: ClassSymbol): Map[String, MethodSymbol] = {
      def inner(map: Map[String, MethodSymbol], left: List[MethodDecl]): Map[String, MethodSymbol] = left match {
        case Nil => map
        case x :: xs => map.get(x.id.value) match {
          case None =>
            val ns = new MethodSymbol(x.id.value, cs)
            ns.setType(fetchType(x.retType))
            ns.setPos(x)
            x.id.setSymbol(ns)
            x.setSymbol(ns)
            inner(map.updated(x.id.value, ns), xs)
          case Some(ms) =>
            fatal(ms.name + " declared twice for class " + cls.id.value, x)
        }
      }
      inner(Map(), cls.methods)
    }

    def collectClasses(prg: Program): Map[String, ClassSymbol] = {
      def inner(map: Map[String, ClassSymbol], cls: List[ClassDecl]): Map[String, ClassSymbol] = {
        cls match {
          case Nil => map
          case x :: xs => map.get(x.id.value) match {
            case None =>
              if (x.id.value == prog.main.id.value) fatal("Class " + x.id.value + " has the same name as the main object")
              val ns = new ClassSymbol(x.id.value)
              ns.setType(fetchType(x.id));
              ns.setPos(x)
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

    def heritageGodMethod(scope: GlobalScope): Unit = {
      def inner(dontwant: Set[ClassSymbol], start: ClassSymbol, youShallNotName: Map[String, VariableSymbol]): Unit = {
        start.parent match {
          case None =>
          case Some(cs) =>
            if (dontwant.contains(cs)) error("Cycle in inheritance graph", cs)
            else {
              val parentVars = cs.members
              val intersect = youShallNotName.keys.toList intersect parentVars.keys.toList
              intersect foreach (overridingField => error("Field " + overridingField + " overrides parent's field", youShallNotName(overridingField)))
              inner(dontwant + cs, cs, parentVars ++: youShallNotName)
            }
        }
      }
      scope.classes.values.foreach { cldecl =>
        inner(Set() + cldecl, cldecl, cldecl.members)
      }
    }

    def checkShadowing(method: MethodSymbol): Unit = {
      val args = method.params.keys toSet
      val vars = method.members.keys toSet

      val intersect = args intersect vars
      intersect foreach (doubleDamage => error("Variable '" + doubleDamage + "' shadows a method parameter in method " + method.name, method.members(doubleDamage)))
    }

    // This is a suggestion:
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    val mainSymb = new ClassSymbol(prog.main.id.value)
    mainSymb.setPos(prog.main.id)
    prog.main.setSymbol(mainSymb)
    prog.main.id.setSymbol(mainSymb)
    globalScope.mainClass = mainSymb

    val classes = collectClasses(prog)
    globalScope.classes = classes

    val methodList = prog.classes flatMap (cldcl => {
      val map = collectMethods(cldcl, cldcl.getSymbol)
      cldcl.getSymbol.methods = map

      map values
    })
    val classVar = prog.classes flatMap (cldcl => {
      val map = collectVariables(cldcl)
      cldcl.getSymbol.members = map
      cldcl.getSymbol.parent = cldcl.parent.flatMap(x => classes.get(x.value))
      map values
    })

    classes.values foreach { cldcl => cldcl.setType(new Types.TObject(cldcl)) }

    val methodVar = (for (classe <- prog.classes) yield {
      classe.methods flatMap { mdcl =>
        {
          val mapMeth = collectMethodVariables(mdcl, mdcl.getSymbol)
          val paramsTuple = collectMethodParam(mdcl, mdcl.getSymbol)
          mdcl.getSymbol.params = paramsTuple._1
          mdcl.getSymbol.argList = paramsTuple._2
          mdcl.getSymbol.members = mapMeth
          (mapMeth ++: paramsTuple._1).values
        }
      }
    }) flatten

    methodList foreach {
      meth => meth.overridden = meth.classSymbol.parent.flatMap(x => x.lookupMethod(meth.name))
    }

    var allSymbols = (methodVar ::: classVar) toSet

    prog.main.stats foreach {
      handleStatTree(_, null)
    }

    prog.classes foreach (
      _.methods foreach (
        meth => meth.stats foreach (handleStatTree(_, meth.getSymbol))))

    prog.classes foreach (cldcl => {
      cldcl.parent match {
        case None =>
        case Some(superType) =>
          if (superType.value == prog.main.id.value) {
            fatal("Cannot inherit the main object", superType)
          }
          attachSymbolToType(superType)
      }
      cldcl.vars foreach (vrdcl => attachSymbolToType(vrdcl.tpe))
      cldcl.methods foreach { meth =>
        meth.vars foreach (vrdcl => attachSymbolToType(vrdcl.tpe))
        attachSymbolToType(meth.retType)
        handleExprTree(meth.retExpr, meth.getSymbol)
        meth.args foreach (arg => attachSymbolToType(arg.tpe))
      }
    })

    def attachSymbolToType(tpe: TypeTree) {
      tpe match {
        case id: Identifier =>
          val ns = globalScope.lookupClass(id.value).getOrElse(fatal(id.value + " undeclared", id))
          ns.setPos(id)
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

    def handleStatTree(st: StatTree, sym: MethodSymbol): Unit = {
      st match {
        case Block(stats) => stats foreach (stats => handleStatTree(stats, sym))
        case If(expr, thn, els) =>
          handleExprTree(expr, sym)
          handleStatTree(thn, sym)
          els match {
            case None =>
            case Some(st) => handleStatTree(st, sym)
          }
        case While(expr, stat) =>
          handleExprTree(expr, sym)
          handleStatTree(stat, sym)
        case WriteLine(expr) => handleExprTree(expr, sym)
        case ShowPopup(expr) => handleExprTree(expr, sym)
        case Assign(id, expr) =>
          attachVarSymbol(id, sym)
          handleExprTree(expr, sym)
        case ArrayAssign(id, index, expr) =>
          handleExprTree(id, sym)
          handleExprTree(index, sym)
          handleExprTree(expr, sym)
      }
    }

    def handleExprTree(et: ExprTree, sym: MethodSymbol): Unit = {
      et match {
        case And(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case Or(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case Plus(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case Minus(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case Times(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case Div(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case LessThan(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case Equals(lhs, rhs) =>
          handleExprTree(lhs, sym)
          handleExprTree(rhs, sym)
        case ArrayRead(arr, index) =>
          handleExprTree(arr, sym)
          handleExprTree(index, sym)
        case ArrayLength(arr) => handleExprTree(arr, sym)
        case MethodCall(obj, meth, args) =>
          handleExprTree(obj, sym)
          // we do nothing with the method id.
          args foreach (expr => handleExprTree(expr, sym))

        case id: Identifier => attachVarSymbol(id, sym)
        case ths: This => ths.setSymbol(sym.classSymbol)

        case NewArray(size, tpe) => {
          handleExprTree(size, sym)
          et.setType(checkArrayType(tpe))
        }

        case New(tpe) => tpe.setSymbol(globalScope.lookupClass(tpe.value).getOrElse(fatal(tpe.value + " not declared", tpe)))
        case Not(expr) => handleExprTree(expr, sym)
        case _ => //TODO
      }
    }

    def checkArrayType(tpe: TypeTree) : TArray = {
      TArray(tpe match {
        case id: Identifier => {
          id.setSymbol(globalScope.lookupClass(id.value).getOrElse(fatal(id.value + " undeclared", tpe))).getType
        }
        case ArrayType(inner) => checkArrayType(inner) //Multi-dimension
        case IntType() => TInt
        case BooleanType() => TBool
        case StringType() => TString
        case DoubleType() => TDouble
      })
    }

    // Checking all other constraints

    // cyclic heritage check & field override check
    heritageGodMethod(globalScope)

    // Method override check
    methodList foreach {
      meth =>
        meth.overridden match {
          case None =>
          case Some(parentMeth) =>
            if (meth.params.size != parentMeth.params.size) {
              error("Overriding method " + meth.name + " has not the right amount of parameters", meth)
            }
        }
    }

    // shadowing check
    classes.values foreach (_.methods.values foreach (checkShadowing(_)))

    // unused resource check
    allSymbols foreach (unused => warning("Unused ressource " + unused.name, unused))

    prog
  }
}
