package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.New
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.{SchemeChangePatterns, SchemeParser}
import maf.util.Reader
import maf.util.benchmarks.Timeout

object ChangeIncremental extends App:
 //val text = Reader.loadFile("test/changeDetectionTest/changeVarName.scm")
 //val text = Reader.loadFile("test/changeDetectionTest/changeDifferentRenamings.scm")
 val text = Reader.loadFile("test/changeDetectionTest/changePar.scm")
 //val text = Reader.loadFile("test/changeDetectionTest/differentTest.scm")
 //val text = Reader.loadFile("test/changes/scheme/fib-loop.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
  val res = CSchemeParser.parseProgram(text)
  println(res)
  val resComps = SchemeChangePatterns.checkForRenamingParameter(res)

 