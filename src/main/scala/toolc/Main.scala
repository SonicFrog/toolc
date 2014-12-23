package toolc

import utils._
import java.io.File
import java.io.PrintWriter

import lexer._
import ast._
import eval._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    val reporter = new Reporter()
    var files: List[File] = Nil
    var outDir: Option[File] = None

    def rec(args: List[String]): Unit = args match {
      case "-d" :: dir :: xs =>
        outDir = Some(new File(dir))
        rec(xs)

      case f :: xs =>
        files  ::= new File(f)
        rec(xs)

      case _ =>
    }

    rec(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val pipeline = Lexer andThen
                   Parser andThen
                   NameAnalysis andThen
                   TypeChecking

    val result = pipeline.run(ctx)(ctx.file)

    ctx.reporter.terminateIfErrors

    generateJSFile(ctx.outDir, result.main.id.value , PrinterJS(result, None))
  }
  
  def generateJSFile(outDir : Option[File], fileName : String, codeContent : String) {
    val dirName = outDir.map(_.getPath + "/").getOrElse("./")
    val f = new File(dirName + fileName + ".js")
    f.delete()
    f.createNewFile();
    
    val pw = new PrintWriter(f)
    
    pw.write(codeContent)
    pw.close
  }
}
