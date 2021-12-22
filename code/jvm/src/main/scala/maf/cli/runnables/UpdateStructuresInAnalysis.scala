
package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Expression
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalUpdateDatastructures}
//import maf.cli.runnables.IncrementalRun.standardTimeout
import maf.core.BasicEnvironment
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambdaExp}
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.scheme.modf.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.{Reader, Writer}
import maf.util.Writer.Writer
import maf.util.benchmarks.Timeout
import maf.util.graph.DotGraph
import maf.util.graph.DotGraph.*
import java.time.LocalDateTime
import scala.concurrent.duration.*

object UpdateStructuresInAnalysis extends App:

  val w = Writer.open("benchOutput/incremental/errors.txt")

  // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
  // Useful when reducing a program when debugging the analysis.
  def interpretProgram(file: String): Unit =
    val prog = CSchemeParser.parseProgram(Reader.loadFile(file))
    val i = new SchemeInterpreter((_, _) => (), stack = true)
    print("*")
    i.run(prog, Timeout.start(Duration(3, MINUTES)), Old)
    print("*")
    i.run(prog, Timeout.start(Duration(3, MINUTES)), New)
    println("*")


  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =
    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
      new IncrementalSchemeModFAnalysisTypeLattice(text, configuration)
        with IncrementalLogging[SchemeExp]
        with IncrementalDataFlowVisualisation[SchemeExp] {
        override def focus(a: Addr): Boolean = a.toString.toLowerCase().nn.contains("ret")

        override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          with IncrementalLoggingIntra
          with IncrementalVisualIntra
      }

    // Analysis from soundness tests.
    def baseNoUpdates(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      //  with SchemeModFFullArgumentSensitivity
      //   with SchemeModFCallSiteSensitivity
      //with SchemeModFFullArgumentCallSiteSensitivity
      with SchemeModFNoSensitivity
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain
      with IncrementalGlobalStore[SchemeExp]
    {
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
    }

    def baseUpdates(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      //  with SchemeModFFullArgumentSensitivity
      //   with SchemeModFCallSiteSensitivity
      // with SchemeModFFullArgumentCallSiteSensitivity
      with SchemeModFNoSensitivity
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain
      with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
    }

    try {
      println(s"***** $bench *****")
      interpretProgram(bench)
      val program = CSchemeParser.parseProgram(Reader.loadFile(bench))

      val analysisWithoutUpdates = baseNoUpdates(program)
      analysisWithoutUpdates.analyzeWithTimeout(timeout())
      val beforeAnalysis = System.nanoTime
      analysisWithoutUpdates.updateAnalysis(timeout())
      val timeAnalysis = System.nanoTime - beforeAnalysis

      val storeWithoutUpdate = analysisWithoutUpdates.store
      val depsWithoutUpdate = analysisWithoutUpdates.deps
      val mappingWithoutUpdate = analysisWithoutUpdates.mapping
      val visitedWithoutUpdate = analysisWithoutUpdates.visited

      println("first analysis done")

      val analysisWithUpdates = baseUpdates(program)
      analysisWithUpdates.analyzeWithTimeout(timeout())
      val beforeUpdateAnalysis = System.nanoTime
      analysisWithUpdates.updateAnalysis(timeout())
      val timeUpdateAnalysis = System.nanoTime - beforeAnalysis

      val storeWithUpdate = analysisWithUpdates.store
      val depsWithUpdate = analysisWithUpdates.deps
      val mappingWithUpdate = analysisWithUpdates.mapping
      val visitedWithUpdate = analysisWithUpdates.visited

      println("updating done")

      println("Time incremental analysis:    " + timeAnalysis)
      println("Time updating:                " + timeUpdateAnalysis)


      println("Store with updating: " + storeWithUpdate.toString)
      println("Store with regular reanalysis: " + storeWithoutUpdate.toString)


      println("store reanalysis -> Update: " + storeWithoutUpdate.forall((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("store update -> reanalysis: " + storeWithUpdate.forall((k, v) =>
        storeWithoutUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)


      storeWithoutUpdate.foreach((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + " " + k.idn.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>
            println("missing in update: " + k.toString() + " " + k.idn.toString() + "\n reanalysis value: " + v.toString)
              k match
              case key: maf.modular.ReturnAddr[_] =>
                key.cmp match
                  case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                    println(lam.toString + " " + env.toString + " " + oldCtx.toString)
              case _ =>
      )


      storeWithUpdate.foreach((k, v) =>
        storeWithoutUpdate.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + " " + k.idn.toString() + "\n update value: " + v.toString)
            k match
            case key: maf.modular.ReturnAddr[_] =>
              key.cmp match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println(lam.toString + " " + env.toString + " " + oldCtx.toString)
            case _ =>
      )

      println()

      println("Dependencies with updating: " + depsWithUpdate.toString)
      println("Dependencies with regular reanalysis: " + depsWithoutUpdate.toString)


      println("Dependencies reanalysis -> Update: " + depsWithoutUpdate.forall((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Dependencies update -> reanalysis: " + depsWithUpdate.forall((k, v) =>
        depsWithoutUpdate.get(k) match
          case Some(updatedValue) => depsWithoutUpdate.==(v)
          case _ => false).toString)

      depsWithoutUpdate.foreach((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>println("missing in update: " + k.toString()  + "\n reanalysis value: " + v.toString))


      depsWithUpdate.foreach((k, v) =>
        depsWithoutUpdate.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + "\n update value: " + v.toString)
      )


      println()

      println("Mapping with updating: " + mappingWithUpdate.toString)
      println("Mapping with regular reanalysis: " + mappingWithoutUpdate.toString)


      println("Mapping reanalysis -> Update: " + mappingWithoutUpdate.forall((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Mapping update -> reanalysis: " + mappingWithUpdate.forall((k, v) =>
        mappingWithoutUpdate.get(k) match
          case Some(updatedValue) => mappingWithoutUpdate.==(v)
          case _ => false).toString)

      mappingWithoutUpdate.foreach((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + " " + k.idn.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>println("missing in update: " + k.toString()  + "\n reanalysis value: " + v.toString))


      mappingWithUpdate.foreach((k, v) =>
        mappingWithoutUpdate.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + "\n update value: " + v.toString)
      )

      println()

      println()

      println("Visited with updating: " + visitedWithUpdate.toString)
      println("Visited with regular reanalysis: " + visitedWithoutUpdate.toString)


      println("Visited reanalysis -> Update: " + visitedWithoutUpdate.forall(e => visitedWithUpdate.contains(e)).toString)

      println("Visited update -> reanalysis: " + visitedWithUpdate.forall(e => visitedWithoutUpdate.contains(e)).toString)

      visitedWithoutUpdate.foreach(e =>
        if !visitedWithUpdate.contains(e) then
          println("missing in update: " + e.toString()))


      visitedWithUpdate.foreach(e =>
        if !visitedWithoutUpdate.contains(e) then
          println("missing in reanalysis: " + e.toString()))

      println()

    } catch {
      case e: Exception =>
        e.printStackTrace(System.out)
        Writer.writeln(w, bench)
        Writer.writeln(w, e.getStackTrace().toString)
        Writer.writeln(w, "")
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm", "test/changeDetectionTest/onlyConsistentRenaming/Vectors.scm", "test/changeDetectionTest/onlyConsistentRenaming/Lists.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/Vectors.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/R5RS/scp1-compressed/7.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/R5RS/gambit/NoSensitivity/peval.scm")
  val modFbenchmarks: List[String] = List("test/changes/scheme/nboyer.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  println("Done")