package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    def typeToJVMType(tpe: Type): String = {
      tpe match {
        case TObject(cs) => "L" + cs.name
        case TInt => "I"
        case TString => "Ljava/lang/String"
        case TBool => "Z"
        case TIntArray => "[I"
        case _ => sys.error("Internal compiler error!")
      }
    }
    
    def typetoJVMPrefixing(tpe: Type): String = {
      tpe match {
        case TObject(_) => "L" 
        case TInt => "I"
        case TString => "L"
        case TBool => "Z"
        case TIntArray => "[I"
        case _ => sys.error("Internal compiler error!")
      }
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile = new ClassFile(ct.id.value, ct.parent.flatMap(x => Some(x.value)))

      classFile.setSourceFile(sourceName)

      ct.vars.foreach {
        x => classFile.addField(typeToJVMType(x.tpe.getType) + ";", x.id.value)
      }

      ct.methods.foreach {
        meth =>
        {
          val retTpe = meth.id.getSymbol.getType
          val argsString = meth.args.map(_.id.getType).map(typeToJVMType(_)).mkString
          val handler = classFile.addMethod(typeToJVMType(retTpe), meth.id.value, argsString).codeHandler

          generateMethodCode(handler, meth)
        }
      }

      classFile.writeToFile(dir)
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      //Mapping from VariableSymbol => Int where Int is the local var slot
      val env: Map[VariableSymbol, Int] = mt.vars.map {
        v => (v.id.getSymbol.asInstanceOf[VariableSymbol], ch.getFreshVar(typeToJVMType(v.id.getType)))
      } toMap
      
      mt.stats foreach { stat => generateStatementCode(ch, stat , env) }

      ch.freeze
    }

    def pushVar(v: VariableSymbol, meth: MethodSymbol, env: Map[VariableSymbol, Int], ch: CodeHandler): Unit = {
      if (env.contains(v)) ch << ILoad(env(v))
      else if (meth.argList.contains(v)) ch << ArgLoad(meth.argList.indexOf(v))
      else ch << GetField(meth.classSymbol.name, v.name, typeToJVMType(v.getType))
    }
    
    def generateStatementCode(ch: CodeHandler, stat: StatTree, env: Map[VariableSymbol, Int]): Unit = {
      stat match {
        case Block(stats) => stats foreach (generateStatementCode(ch, _, env))
        case If(cond, thn, els) => {
          generateExpressionCode(ch, cond, env)
          generateStatementCode(ch, thn, env)
          els foreach (generateStatementCode(ch, _, env))
          
          ???
        }
        
        case While(cond, stat) => {
          generateExpressionCode(ch, cond, env)
          generateStatementCode(ch, stat, env)
          
          ???
        }
        
        case Println(expr) => {
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateExpressionCode(ch, expr, env)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
          RETURN
        }
        
        case Assign(id, expr) => {
          generateExpressionCode(ch, expr, env)
          ???
          //ch << typetoJVMPrefixing(id.getType) match + Store(env(id.getSymbol))
        }
        
        case ArrayAssign(id, index, expr) => {
          ???
          
          generateExpressionCode(ch, expr, env)
        }
      }
    }

    def generateExpressionCode(ch: CodeHandler, expr: ExprTree, env: Map[VariableSymbol, Int]): Unit = {
      expr match {
        case Equals(lhs, rhs) => {
          generateExpressionCode(ch, lhs, env)
          generateExpressionCode(ch, rhs, env)

          ???
        }

        case Plus(lhs, rhs) => {
          generateExpressionCode(ch, lhs, env)
          generateExpressionCode(ch, rhs, env)
          
          ???
        }

        case Minus(lhs, rhs) => ???
        case Times(lhs, rhs) => ???
        case Div(lhs, rhs) => ???
        case IntLit(value) => ???
        case LessThan(lhs, rhs) => ???
        case New(tpe) => ch << DefaultNew(typeToJVMType(tpe.getType))
        case Not(bool) => ???
        case This() => ???
        case StringLit(value) => ???
        case True() => ???
        case False() => ???

        case MethodCall(obj, meth, args) => {
          generateExpressionCode(ch, obj, env)
          //TODO: push arguments on the stack
          ch << InvokeVirtual(typeToJVMType(obj.getType), meth.value, "("+args.map(x => typeToJVMType(x.getType)).mkString+")" + typeToJVMType(expr.getType))
        }

        case ArrayLength(arr) => {
          generateExpressionCode(ch, arr, env)
          ch << ARRAYLENGTH
        }

        case ArrayRead(arr, index) => {
          generateExpressionCode(ch, arr, env)
          generateExpressionCode(ch, index, env)
          ch << IALOAD
        }

        case NewIntArray(size) => ch << NewArray("I")

        case Identifier(value) => ???

        case Or(lhs, rhs) => ???
        case And(lhs, rhs) => ???
      }
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      // TODO: Emit code
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    // ...
  }

}
