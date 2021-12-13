package maf.cli.runnables

import maf.core.Expression
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.New
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.{SchemeChangePatterns, SchemeCodeChange, SchemeExp, SchemeLambdaExp, SchemeLet, SchemeLetrec, SchemeLettishExp, SchemeParser, SchemeRenamer}
import maf.util.Reader
import maf.util.benchmarks.Timeout
import smtlib.extensions.tip.Terms.Lambda
import maf.language.change.ChangeExp

object ChangeIncremental extends App:
 val text = Reader.loadFile("test/R5RS/gambit/array1.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
  var fullParsed = SchemeParser.parseProgram(text)
  val parsed = SchemeParser.parse(text)
  println(fullParsed.prettyString())
  println()
  val renamableSubexpr = parsed.flatMap(findAllRenamableExps(_))
  val renamedSubexpr = renamableSubexpr.map(SchemeParser.rename(_))
  val renamableToRenamer = renamableSubexpr.zip(renamedSubexpr).toMap[Expression, Expression]
  println(fullParsed.getClass)
  renamableSubexpr.foreach(e => replaceInFull(e))
  println(fullParsed.prettyString())
  //parsed.foreach(println(_))

 /* println(fullParsed.subexpressions)
  println(renamableSubexpr)


  println(renamableSubexpr.map(SchemeParser.rename(_)))*/

  private def replaceInFull(toFind: SchemeExp) : Unit = fullParsed match {
    case letrec: SchemeLetrec =>
      var newBindings = letrec.bindings.map(binding =>
        if binding._2.eql(toFind) then
          (toFind, renamableToRenamer.getOrElse(toFind, toFind)) match
            case (toFind: SchemeExp, renamed: SchemeExp) =>
              (binding._1, SchemeCodeChange(old = toFind, nw = renamed, idn = toFind.idn))
        else binding
      )
      var newBody = letrec.body.map(e =>
        if e.eql(toFind) then
          (toFind, renamableToRenamer.getOrElse(toFind, toFind)) match
            case (toFind: SchemeExp, renamed: SchemeExp) =>
              SchemeCodeChange(old = toFind, nw = renamed, idn = toFind.idn)
        else e)
      fullParsed = SchemeLetrec(newBindings, newBody, fullParsed.idn)

  }

  private def findAllRenamableExps(expr: Expression): List[SchemeExp] = expr match
    case lam: SchemeLambdaExp  => List(lam)
    case let: SchemeLettishExp => List(let)
    case expr if expr.subexpressions.isEmpty && expr.height == 1 => List()
    case expr if  expr.subexpressions.isEmpty => List()
    case _ => List().appendedAll(expr.subexpressions.flatMap(e => findAllRenamableExps(e)))



 