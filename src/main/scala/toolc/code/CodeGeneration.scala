package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._
import com.sun.org.apache.bcel.internal.generic.ICONST

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

      mt.stats foreach { stat => generateStatementCode(ch, mt, stat , env) }

      ch.freeze
    }

    def pushVar(v: VariableSymbol, meth: MethodSymbol, env: Map[VariableSymbol, Int], ch: CodeHandler): Unit = {
      if (env.contains(v)) {
        val jVMSlot = env(v);
        ch << (v.getType match {
          case TInt | TBool => ILoad(jVMSlot)
          case TObject(_) | TString => LLoad(jVMSlot)
          case TIntArray => ALoad(jVMSlot)
          case _ => sys.error("Internal compiler error!")
        })
      }
      else if (meth.argList.contains(v)) ch << ArgLoad(meth.argList.indexOf(v))
      else ch << GetField(meth.classSymbol.name, v.name, typeToJVMType(v.getType))
    }

    def generateStatementCode(ch: CodeHandler, mt: MethodDecl, stat: StatTree, env: Map[VariableSymbol, Int]): Unit = {
      stat match {
        case Block(stats) => stats foreach (generateStatementCode(ch, mt, _, env))
        case If(cond, thn, els) => {
          val labelElse = ch.getFreshLabel("else")
          val labelEndIf = ch.getFreshLabel("endIf")

          generateExpressionCode(ch, mt, cond, env)

          ch << Ldc(1) << If_ICmpNe(labelElse)

          generateStatementCode(ch, mt, thn, env)

          ch << Goto(labelEndIf) << Label(labelElse)
          els foreach (generateStatementCode(ch, mt, _, env))

          ch << Label(labelEndIf)
        }

        case While(cond, stat) => {
          val labelStartLoop = ch.getFreshLabel("loopStart")
          val labelEndLoop = ch.getFreshLabel("loopEnd")

          ch << Label(labelStartLoop)

          generateExpressionCode(ch, mt, cond, env)

          ch << Ldc(1) << If_ICmpNe(labelEndLoop)

          generateStatementCode(ch, mt, stat, env)

          ch << Goto(labelStartLoop) << Label(labelEndLoop)

        }

        case Println(expr) => {
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateExpressionCode(ch, mt, expr, env)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
        }

        case Assign(id, expr) => {
          generateExpressionCode(ch, mt, expr, env)

          val jVMSlot = env(id.getSymbol.asInstanceOf[VariableSymbol])

          ch << (id.getType match {
            case TObject(_) =>  LStore(jVMSlot)
            case TInt =>  IStore(jVMSlot)
            case TString =>  LStore(jVMSlot)
            case TBool =>  IStore(jVMSlot)
            case TIntArray =>  AStore(jVMSlot)
            case _ => sys.error("Internal compiler error!")
          })
        }

        case ArrayAssign(id, index, expr) => {
          generateExpressionCode(ch, mt, id, env)

          generateExpressionCode(ch, mt, index, env)

          generateExpressionCode(ch, mt, expr, env)

          ch << IASTORE
        }
      }
    }

    def generateExpressionCode(ch: CodeHandler, mt: MethodDecl, expr: ExprTree, env: Map[VariableSymbol, Int]): Unit = {
      expr match {
        case Equals(lhs, rhs) => {
          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          val labelAfter = ch.getFreshLabel("after")
          val labelTrue = ch.getFreshLabel("true")

          lhs.getType match {
            case TInt | TBool => ch << If_ICmpEq(labelTrue)
            case TIntArray | TObject(_) | TString => ch << If_ACmpEq(labelTrue)
            case _ => sys.error("Internal compiler error!")
          }

          ch << Ldc(0) << Goto(labelAfter) << Label(labelTrue) << Ldc(1) << Label(labelAfter)
        }

        case Plus(lhs, rhs) => {
          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          ???
        }

        case Minus(lhs, rhs) => {
          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          ch << ISUB
        }

        case Times(lhs, rhs) => {
          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          ch << IMUL
        }

        case Div(lhs, rhs) => {
          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          ch << IDIV
        }

        case IntLit(value) => ch << Ldc(value)

        case LessThan(lhs, rhs) => ???
        case New(tpe) => ch << DefaultNew(typeToJVMType(tpe.getType))
        case Not(bool) => {
          generateExpressionCode(ch, mt, bool, env)

          val labelAfter = ch.getFreshLabel("after")
          val labelTrue = ch.getFreshLabel("true")

          ch << Ldc(0) << If_ICmpEq(labelTrue) << Ldc(1) << Goto(labelAfter) << Label(labelTrue) << Ldc(0) << Label(labelAfter)
        }

        case This() => ch << ArgLoad(0)
        case StringLit(value) => ch << Ldc(value)
        case True() => ch << Ldc(1)
        case False() => ch << Ldc(0)

        case MethodCall(obj, meth, args) => {
          generateExpressionCode(ch, mt, obj, env)
          //TODO: push arguments on the stack
          ch << InvokeVirtual(typeToJVMType(obj.getType), meth.value, "("+args.map(x => typeToJVMType(x.getType)).mkString+")" + typeToJVMType(expr.getType))
        }

        case ArrayLength(arr) => {
          generateExpressionCode(ch, mt, arr, env)
          ch << ARRAYLENGTH
        }

        case ArrayRead(arr, index) => {
          generateExpressionCode(ch, mt, arr, env)
          generateExpressionCode(ch, mt, index, env)
          ch << IALOAD
        }

        case NewIntArray(size) => ch << NewArray("I")

        case id : Identifier => pushVar(id.getSymbol.asInstanceOf[VariableSymbol], mt.getSymbol , env, ch)

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
