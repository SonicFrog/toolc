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
        case TObject(cs) => "L" + cs.name + ";"
        case TInt => "I"
        case TString => "Ljava/lang/String;"
        case TBool => "Z"
        case TIntArray => "[I"
        case _ => sys.error("Internal compiler error!")
      }
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile = new ClassFile(ct.id.value, ct.parent.flatMap(x => Some(x.value)))

      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      ct.vars.foreach {
        x => classFile.addField(typeToJVMType(x.id.getType), x.id.value)
      }

      ct.methods.foreach {
        meth => {
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

      ch << LineNumber(mt.retExpr.line)
      generateExpressionCode(ch, mt, mt.retExpr, env)

      ch << (mt.retExpr.getType match {
        case TString | TIntArray | TObject(_) => ARETURN
        case TBool | TInt => IRETURN
        case _ => sys.error("Internal compiler error!")
      })

      ch.freeze
    }

    def pushVar(v: VariableSymbol, meth: MethodSymbol, env: Map[VariableSymbol, Int], ch: CodeHandler): Unit = {
      if (env.contains(v)) {
        val jVMSlot = env(v);
        ch << (v.getType match {
          case TInt | TBool => ILoad(jVMSlot)
          case TObject(_) | TString | TIntArray => ALoad(jVMSlot)
          case _ => sys.error("Internal compiler error!")
        })
      }
      else if (meth.argList.contains(v)) ch << ArgLoad(meth.argList.indexOf(v) + 1)
      else ch << ALoad(0) << GetField(meth.classSymbol.name, v.name, typeToJVMType(v.getType))
    }

    def generateStatementCode(ch: CodeHandler, mt: MethodDecl, stat: StatTree, env: Map[VariableSymbol, Int]): Unit = {
      ch << LineNumber(stat.line)
      stat match {
        case Block(stats) => stats foreach (generateStatementCode(ch, mt, _, env))
        case If(cond, thn, els) => {
          val end = ch.getFreshLabel("if_end")

          generateExpressionCode(ch, mt, cond, env)

          if (els.isEmpty) {
            ch << IfEq(end)

            generateStatementCode(ch, mt, thn, env)
          }
          else {
            val els_lab = ch.getFreshLabel("if_else")
            ch << IfEq(els_lab)

            generateStatementCode(ch, mt, thn, env)

            ch << Goto(end)

            ch << Label(els_lab)

            generateStatementCode(ch, mt, els.get, env)
          }

          ch << Label(end)
        }

        case While(cond, stat) => {
          val begin = ch.getFreshLabel("loopStart")
          val end = ch.getFreshLabel("loopEnd")

          ch << Label(begin)

          generateExpressionCode(ch, mt, cond, env)

          ch << IfEq(end)

          generateStatementCode(ch, mt, stat, env)

          ch << Goto(begin) << Label(end)
        }

//        case Println(expr) => {
//          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
//          generateExpressionCode(ch, mt, expr, env)
//          ch << InvokeVirtual("java/io/PrintStream", "println", "(" + typeToJVMType(expr.getType) + ")V")
//        }

        case Assign(id, expr) => {

          val v = id.getSymbol.asInstanceOf[VariableSymbol]
          val jVMSlot = env.get(v)

          if (jVMSlot.isDefined) {
            generateExpressionCode(ch, mt, expr, env)
            ch << (v.getType match {
              case TInt | TBool => IStore(jVMSlot.get)
              case TObject(_) | TString | TIntArray => AStore(jVMSlot.get)
              case _ => sys.error("Internal compiler error!")
            })
          }
          else if (mt.getSymbol.argList.contains(v)) {
            generateExpressionCode(ch, mt, expr, env)
            ch << (v.getType match {
              case TInt | TBool => IStore(mt.getSymbol.argList.indexOf(v) + 1)
              case TObject(_) | TString | TIntArray => AStore(mt.getSymbol.argList.indexOf(v) + 1)
              case _ => sys.error("Internal compiler error!")
            })
          }
          else {
            ch << ALoad(0)

            generateExpressionCode(ch, mt, expr, env)

            ch << PutField(mt.getSymbol.classSymbol.name, v.name, typeToJVMType(v.getType))
          }
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
          val labelAfter = ch.getFreshLabel("after")

          ch << Ldc(1)

          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          lhs.getType match {
            case TInt | TBool => ch << If_ICmpEq(labelAfter)
            case TIntArray | TObject(_) | TString => ch << If_ACmpEq(labelAfter)
            case _ => sys.error("Internal compiler error!")
          }

          ch << POP << Ldc(0) << Label(labelAfter)
        }

        case Plus(lhs, rhs) => {

          if (lhs.getType == TInt && rhs.getType == TInt) {
            generateExpressionCode(ch, mt, lhs, env)
            generateExpressionCode(ch, mt, rhs, env)
            ch << IADD
          } else {
            ch << DefaultNew("java/lang/StringBuilder")
            generateExpressionCode(ch, mt, lhs, env)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToJVMType(lhs.getType) + ")"+ "Ljava/lang/StringBuilder;")
            generateExpressionCode(ch, mt, rhs, env)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + typeToJVMType(rhs.getType) + ")"+ "Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          }
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

        case LessThan(lhs, rhs) => {
          val labelAfter = ch.getFreshLabel("after")

          ch << Ldc(1)

          generateExpressionCode(ch, mt, lhs, env)
          generateExpressionCode(ch, mt, rhs, env)

          ch << If_ICmpLt(labelAfter) << POP << Ldc(0) << Label(labelAfter)
        }

        case New(tpe) => ch << DefaultNew(tpe.value)
        case Not(bool) => {
          val end = ch.getFreshLabel("not_end")

          generateExpressionCode(ch, mt, bool, env)

          ch << Ldc(1)
          ch << SWAP
          ch << IfEq(end)
          ch << POP
          ch << Ldc(0)
          ch << Label(end)
        }


        case This() => ch << ArgLoad(0)
        case StringLit(value) => ch << Ldc(value)
        case True() => ch << Ldc(1)
        case False() => ch << Ldc(0)

        case MethodCall(obj, meth, args) => {
          generateExpressionCode(ch, mt, obj, env)
          args.foreach(x => generateExpressionCode(ch, mt, x, env))
          val sym = meth.getSymbol.asInstanceOf[MethodSymbol]

          ch << InvokeVirtual(sym.classSymbol.name, meth.value, "("+sym.argList.map(x => typeToJVMType(x.getType)).mkString+")" + typeToJVMType(expr.getType))
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

//        case NewArray(size) => {
//          generateExpressionCode(ch, mt, size, env)
//          ch << NewArray(10) // 10 sets type to Int
//        }

        case id : Identifier => pushVar(id.getSymbol.asInstanceOf[VariableSymbol], mt.getSymbol , env, ch)

        case Or(lhs, rhs) => {
          val end = ch.getFreshLabel("end_or")
          ch << Ldc(1)

          generateExpressionCode(ch, mt, lhs, env)

          ch << IfNe(end)

          ch << POP

          generateExpressionCode(ch, mt, rhs, env)

          ch << Label(end)
        }

        case And(lhs, rhs) => {
          val end = ch.getFreshLabel("end_and")

          ch << Ldc(0)

          generateExpressionCode(ch, mt, lhs, env)

          ch << IfEq(end)

          ch << POP

          generateExpressionCode(ch, mt ,rhs, env)

          ch << Label(end)
        }
      }
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir + "/" + ct.id.value + ".class")
    }

    val mainClass = new ClassFile(prog.main.id.value, None)

    mainClass.setSourceFile(sourceName)
    mainClass.addDefaultConstructor

    val handler = mainClass.addMainMethod.codeHandler


    prog.main.stats foreach {
      generateStatementCode(handler, null, _, Map())
    }

    handler << RETURN

    handler.freeze

    mainClass.writeToFile(outDir + "/" + prog.main.id.value + ".class")
  }
}
