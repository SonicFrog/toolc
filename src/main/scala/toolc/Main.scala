package toolc

import utils._
import java.io.File
import java.io.PrintWriter

import lexer._
import ast._
import analyzer._

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

    val scriptJS = generateJSFile(ctx.outDir, result.main.id.value , PrinterJS(result, None))
    generateHTMLFromJS(ctx.outDir, "test_tool", scriptJS)
  }
  
  def generateJSFile(outDir : Option[File], fileName : String, codeContent : String) : File = {
    val dirName = outDir.map(_.getPath + "/").getOrElse("./")
    val f = new File(dirName + fileName + ".js")
    f.delete()
    f.createNewFile();
    
    val pw = new PrintWriter(f)
    
    pw.write(codeContent)
    pw.close
    
    f
  }
  
  def generateHTMLFromJS(outDir: Option[File], fileName : String, script: File) {
    val dirName = outDir.map(_.getPath + "/").getOrElse("./")
    val f = new File(dirName + fileName + ".html")
    f.delete()
    f.createNewFile();
    
    val pw = new PrintWriter(f)
    
    pw.write("<!DOCTYPE html>\n<html>\n    <head><meta content=\"text/html; charset=UTF-8\" http-equiv=\"Content-Type\"></meta></head>\n    <body>\n        <pre STYLE=\"font-family: Menlo\"><script src=\"" + script.toPath().toString() + "\"></script></pre>\n    </body>\n</html>")
    pw.close
  }
}
