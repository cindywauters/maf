package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Expression
import maf.language.scheme.SchemeCodeChange
import maf.modular.{AddrDependency, Dependency}
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.update.{UpdateIncrementalSchemeModFBigStepSemantics, IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdate, IncrementalModAnalysisWithUpdateTwoVersions, IncrementalUpdateDatastructures}
import maf.modular.scheme.SchemeAddr
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
import maf.modular.incremental.update.SchemeModFSemanticsUpdate

object RenamingTester extends App:

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =

    def baseUpdates(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
      with StandardSchemeModFComponents
      // with SchemeModFFullArgumentSensitivity
      //with SchemeModFCallSiteSensitivity
      //with SchemeModFFullArgumentCallSiteSensitivity
      with SchemeModFNoSensitivity
      with SchemeModFSemanticsUpdate
      with LIFOWorklistAlgorithm[SchemeExp]
      with UpdateIncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain
      with IncrementalModAnalysisWithUpdateTwoVersions(newProgram)
      with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp) with UpdateIncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis    }

    try {
      println(s"***** $bench *****")
      val program = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(bench))//CSchemeParser.parseProgram(Reader.loadFile(bench))

      println(program._1.prettyString())
      println(program._2.prettyString())

      val analysisWithUpdates = baseUpdates(program._1, program._2)

      analysisWithUpdates.analyzeWithTimeout(timeout())
      val beforeUpdateAnalysis = System.nanoTime
      analysisWithUpdates.version = New
      analysisWithUpdates.updateAnalysis(timeout())
      val timeUpdateAnalysis = System.nanoTime - beforeUpdateAnalysis

      val storeWithUpdate = analysisWithUpdates.store
      val depsWithUpdate = analysisWithUpdates.deps
      val mappingWithUpdate = analysisWithUpdates.mapping
      val visitedWithUpdate = analysisWithUpdates.visited


      val analysisWithoutUpdates = baseUpdates(program._1, program._2)
      analysisWithoutUpdates.version = New
      analysisWithoutUpdates.analyzeWithTimeout(timeout())

      val storeWithoutUpdate = analysisWithoutUpdates.store
      val depsWithoutUpdate = analysisWithoutUpdates.deps
      val mappingWithoutUpdate = analysisWithoutUpdates.mapping
      val visitedWithoutUpdate = analysisWithoutUpdates.visited

      println("updating done")

      println("Time updating:                " + timeUpdateAnalysis)

      println("Store with update: " + storeWithUpdate.toString)
      println("Store new only   : " + storeWithoutUpdate.toString)

      println("store reanalysis -> Update (subsumption): " + storeWithoutUpdate.forall((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) => analysisWithUpdates.lattice.subsumes(updatedValue, v)
          case _ =>
            println("old: " + v.toString + " " + k.toString())
            false).toString)

      println("Dependencies with update: " + depsWithUpdate.toString)
      println("Dependencies new only   : " + depsWithoutUpdate.toString)

      println("Dependencies reanalysis -> Update (subsumption): " + depsWithoutUpdate.forall((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v) || v.forall(elv => updatedValue.contains(elv))
          case _ => false).toString)

      println("Mapping with update : " + mappingWithUpdate.toString)
      println("Mapping new only    : " + mappingWithoutUpdate.toString)

      println("Mapping reanalysis -> Update (subsumption): " + mappingWithoutUpdate.forall((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v) || v.forall(elv => updatedValue.contains(elv))
          case _ => false).toString)

      println("Visited with update : " + visitedWithUpdate.toString)
      println("Visited new only    : " + visitedWithoutUpdate.toString)
      println("Visited reanalysis -> Update (subsumption): " + visitedWithoutUpdate.forall(e => visitedWithUpdate.contains(e)).toString)

      println(storeWithUpdate.size)
      println(depsWithUpdate.size)
      println(mappingWithUpdate.size)
      println(visitedWithUpdate.size)


    } catch {
      case e: Exception =>
        println(e)
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changeDetectionTest/testsWithUpdate/updateTestFile.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  println("Done")