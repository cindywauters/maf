package maf.cli.runnables

import maf.core.{Expression, Identifier, Identity}
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.New
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.{SchemeChangePatterns, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeLambda, SchemeLambdaExp, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp, SchemeParser, SchemeRenamer, SchemeVarArgLambda}
import maf.util.Reader
import maf.util.benchmarks.Timeout
import smtlib.extensions.tip.Terms.Lambda
import maf.language.change.ChangeExp
import scala.util.Random

object ChangeIncremental extends App:
  val rand = scala.util.Random
  val text = Reader.loadFile("test/R5RS/gambit/array1.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
  val parsed = SchemeParser.parse(text)
  val renamableSubexpr = parsed.flatMap(findSomeRenamableExps(_))
  val renamedSubexpr = renamableSubexpr.map(SchemeParser.rename(_))
  val renamableToRenamer = renamableSubexpr.zip(renamedSubexpr).toMap[SchemeExp, SchemeExp]
  parsed.foreach(e => println(replaceInParsed(e).prettyString()))


  private def findSomeRenamableExps(expr: Expression): List[SchemeExp] = expr match
    case lam: SchemeLambdaExp  =>
      val random = rand.nextInt(10)
      if random < 5 then
        List(lam)
      else
        List().appendedAll(lam.subexpressions.flatMap(e => findSomeRenamableExps(e)))
    case let: SchemeLettishExp =>
      val random = rand.nextInt(10)
      if random < 5 then
        List(let)
      else
        List().appendedAll(let.subexpressions.flatMap(e => findSomeRenamableExps(e)))
    case expr if expr.subexpressions.isEmpty && expr.height == 1 => List()
    case expr if  expr.subexpressions.isEmpty => List()
    case _ => List().appendedAll(expr.subexpressions.flatMap(e => findSomeRenamableExps(e)))

  private def replaceInParsed(parsed: SchemeExp): SchemeExp = parsed match {
    case define: SchemeDefineVariable =>
      SchemeDefineVariable(define.name, replaceInParsed(define.value), define.idn)
    case lambda: SchemeLambda =>
      if renamableSubexpr contains lambda then
        val nw = renamableToRenamer.getOrElse(lambda, lambda)
        SchemeCodeChange(old = lambda, nw = nw, idn = lambda.idn)
      else
        SchemeLambda(lambda.name, lambda.args, lambda.body.map(replaceInParsed(_)), lambda.idn)
    case lambda: SchemeVarArgLambda =>
      if renamableSubexpr contains lambda then
        val nw = renamableToRenamer.getOrElse(lambda, lambda)
        SchemeCodeChange(old = lambda, nw = nw, idn = lambda.idn)
      else
        SchemeVarArgLambda(lambda.name, lambda.args, lambda.vararg, lambda.body.map(replaceInParsed(_)), lambda.idn)
    case let: SchemeLet =>
      if renamableSubexpr contains let then
        val nw = renamableToRenamer.getOrElse(let, let)
        SchemeCodeChange(old = let, nw = nw, idn = let.idn)
      else
        SchemeLet(let.bindings.map(b => (b._1, replaceInParsed(b._2))), let.body.map(replaceInParsed(_)), let.idn)
    case let: SchemeLetStar =>
      if renamableSubexpr contains let then
        val nw = renamableToRenamer.getOrElse(let, let)
        SchemeCodeChange(old = let, nw = nw, idn = let.idn)
      else
        SchemeLetStar(let.bindings.map(b => (b._1, replaceInParsed(b._2))), let.body.map(replaceInParsed(_)), let.idn)
   case let: SchemeLetrec =>
      if renamableSubexpr contains let then
        val nw = renamableToRenamer.getOrElse(let, let)
        SchemeCodeChange(old = let, nw = nw, idn = let.idn)
      else
        SchemeLetrec(let.bindings.map(b => (b._1, replaceInParsed(b._2))), let.body.map(replaceInParsed(_)), let.idn)
    case _ =>
      parsed
  }
