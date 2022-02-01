package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.{Expression, Identifier, Identity}
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.New
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.{SchemeChangePatterns, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp, SchemeParser, SchemeRenamer, SchemeVarArgLambda, SchemeFuncall}
import maf.util.Reader
import maf.util.benchmarks.Timeout
import smtlib.extensions.tip.Terms.Lambda
import maf.language.change.ChangeExp

import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Random

object GenerateConsistentRenamings extends App:
  val rand = scala.util.Random
  val gambit = SchemeBenchmarkPrograms.gambit
  val gambitSkip = List(
    "test\\R5RS\\gambit\\compiler.scm",
    "test\\R5RS\\gambit\\sboyer.scm",
    "test\\R5RS\\gambit\\nboyer.scm",
    "test\\R5RS\\gambit\\scheme.scm",
    "test\\R5RS\\gambit\\slatex.scm", // this file and up: var arg lambdas - renamer problem
    "test\\R5RS\\gambit\\fibc.scm", // Contains call cc: java.lang.Exception: Undefined variable call/cc at position call-with-current-continuation:1:40.
    "test\\R5RS\\gambit\\puzzle.scm")
  //runForAllBenchmarks(gambit, gambitSkip, 45)

  val ad = SchemeBenchmarkPrograms.ad
  val adSkip = List(
    "test\\R5RS\\ad\\RBtreeADT.scm", // var arg lambdas
    "test\\R5RS\\ad\\stspaceCODE.scm", // cannot parse expression
    "test\\R5RS\\ad\\bfirst.scm",
    "test\\R5RS\\ad\\linear.scm",
    "test\\R5RS\\ad\\queue.scm",
    "test\\R5RS\\ad\\list.scm",
    "test\\R5RS\\ad\\bst.scm"
  )
 // runForAllBenchmarks(ad, adSkip, 45)

  val tls = SchemeBenchmarkPrograms.theLittleSchemer
  val tlsSkip = List(
    "test\\R5RS\\WeiChenRompf2019\\the-little-schemer\\ch9.scm"
  )
 // runForAllBenchmarks(tls, tlsSkip, 45)

  val scp1 = SchemeBenchmarkPrograms.scp1
  val scp1Skip = List(
    "test\\R5RS\\scp1\\parking-counter.scm",
    "test\\R5RS\\scp1\\twitter.scm"
  )
  //runForAllBenchmarks(scp1, scp1Skip, 50)

  val scp1c = SchemeBenchmarkPrograms.scp1_compressed
  val scp1CSkip = List(
    "test\\R5RS\\scp1-compressed\\all.scm",
    "test\\R5RS\\scp1-compressed\\8.scm"
  )
  //runForAllBenchmarks(scp1c, scp1CSkip, 90)

  val various = SchemeBenchmarkPrograms.various
  val variousSkip = List(
    "test\\R5RS\\various\\pico.scm",
    "test\\R5RS\\various\\Streams.scm",
    "test\\R5RS\\various\\quasiquoting.scm"
  )
 // runForAllBenchmarks(various, variousSkip, 50)


  private def runForAllBenchmarks(bench: Set[String], skipSet: List[String], chance: Int): Unit =
    bench.foreach(benchfile =>
      if !skipSet.contains(benchfile) then
        var filename = benchfile.split("\\\\")
        if filename != null then
          println(benchfile)
          val text = Reader.loadFile(benchfile)
          val interpreter = new SchemeInterpreter((_, _) => (), stack = true)
          val parsed = SchemeParser.parse(text)
          val renamableSubexpr = parsed.flatMap(findSomeRenamableExps(_, chance))
          val renamedSubexpr = renamableSubexpr.map(SchemeParser.rename(_))
          val renamableToRenamer = renamableSubexpr.zip(renamedSubexpr).toMap[SchemeExp, SchemeExp]

          val file = new File("test/changeDetectionTest/onlyConsistentRenaming/R5RS/" + filename(2) + "/" + filename(3))
          val bw = new BufferedWriter(new FileWriter(file))
          bw.write(";; renamed lambdas/lets: " + renamedSubexpr.length.toString + "\n \n")
          parsed.foreach(e => bw.write(replaceInParsed(e, renamableSubexpr, renamableToRenamer).prettyString().concat("\n \n")))
          bw.close()
      )

  private def findSomeRenamableExps(expr: Expression, chance: Int): List[SchemeExp] = expr match
    case lam: SchemeLambdaExp  =>
      val random = rand.nextInt(100)
      if random < chance then
        List(lam)
      else
        List().appendedAll(lam.subexpressions.flatMap(e => findSomeRenamableExps(e, chance)))
    case let: SchemeLettishExp =>
      val random = rand.nextInt(100)
      if random < chance then
        List(let)
      else
        List().appendedAll(let.subexpressions.flatMap(e => findSomeRenamableExps(e, chance)))
    case expr if expr.subexpressions.isEmpty && expr.height == 1 => List()
    case expr if  expr.subexpressions.isEmpty => List()
    case _ => List().appendedAll(expr.subexpressions.flatMap(e => findSomeRenamableExps(e, chance)))

  private def replaceInParsed(parsed: SchemeExp, renamableSubexpr: List[SchemeExp], renamableToRenamer: Map[SchemeExp, SchemeExp]): SchemeExp = parsed match {
    case define: SchemeDefineVariable =>
      SchemeDefineVariable(define.name, replaceInParsed(define.value, renamableSubexpr, renamableToRenamer), define.idn)
    case lambda: SchemeLambda =>
      if renamableSubexpr contains lambda then
        val nw = renamableToRenamer.getOrElse(lambda, lambda)
        SchemeCodeChange(old = lambda, nw = nw, idn = lambda.idn)
      else
        SchemeLambda(lambda.name, lambda.args, lambda.body.map(replaceInParsed(_, renamableSubexpr, renamableToRenamer)), lambda.annotation, lambda.idn)
    case lambda: SchemeVarArgLambda =>
      if renamableSubexpr contains lambda then
        val nw = renamableToRenamer.getOrElse(lambda, lambda)
        SchemeCodeChange(old = lambda, nw = nw, idn = lambda.idn)
      else
        SchemeVarArgLambda(lambda.name, lambda.args, lambda.vararg, lambda.body.map(replaceInParsed(_, renamableSubexpr, renamableToRenamer)), lambda.annotation, lambda.idn)
    case let: SchemeLet =>
      if renamableSubexpr contains let then
        val nw = renamableToRenamer.getOrElse(let, let)
        SchemeCodeChange(old = let, nw = nw, idn = let.idn)
      else
        SchemeLet(let.bindings.map(b => (b._1, replaceInParsed(b._2, renamableSubexpr, renamableToRenamer))), let.body.map(replaceInParsed(_, renamableSubexpr, renamableToRenamer)), let.idn)
    case let: SchemeLetStar =>
      if renamableSubexpr contains let then
        val nw = renamableToRenamer.getOrElse(let, let)
        SchemeCodeChange(old = let, nw = nw, idn = let.idn)
      else
        SchemeLetStar(let.bindings.map(b => (b._1, replaceInParsed(b._2, renamableSubexpr, renamableToRenamer))), let.body.map(replaceInParsed(_, renamableSubexpr, renamableToRenamer)), let.idn)
    case let: SchemeLetrec =>
      if renamableSubexpr contains let then
        val nw = renamableToRenamer.getOrElse(let, let)
        SchemeCodeChange(old = let, nw = nw, idn = let.idn)
      else
        SchemeLetrec(let.bindings.map(b => (b._1, replaceInParsed(b._2, renamableSubexpr, renamableToRenamer))), let.body.map(replaceInParsed(_, renamableSubexpr, renamableToRenamer)), let.idn)
    case ifExp: SchemeIf =>
      SchemeIf(replaceInParsed(ifExp.cond, renamableSubexpr, renamableToRenamer), replaceInParsed(ifExp.cons, renamableSubexpr, renamableToRenamer), replaceInParsed(ifExp.alt, renamableSubexpr, renamableToRenamer), ifExp.idn)
    case fun: SchemeFuncall =>
      SchemeFuncall(replaceInParsed(fun.f, renamableSubexpr, renamableToRenamer), fun.args.map(a => replaceInParsed(a, renamableSubexpr, renamableToRenamer)), fun.idn)
    case _ =>
    //  println(parsed.getClass)
      parsed
  }
