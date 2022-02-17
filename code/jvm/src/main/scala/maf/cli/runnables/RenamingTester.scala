package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Expression
import maf.modular.{AddrDependency, Dependency}
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalUpdateDatastructures}
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

object RenamingTester extends App:

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

    def baseUpdates(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      // with SchemeModFFullArgumentSensitivity
      //with SchemeModFCallSiteSensitivity
      //with SchemeModFFullArgumentCallSiteSensitivity
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
      val program = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(bench))//CSchemeParser.parseProgram(Reader.loadFile(bench))

      println(program._1.prettyString())
      println(program._2.prettyString())

      val analysisWithUpdates = baseUpdates(program._1)
      analysisWithUpdates.analyzeWithTimeout(timeout())

      val storeBefore = analysisWithUpdates.store
      val depsBefore = analysisWithUpdates.deps
      val mappingBefore = analysisWithUpdates.mapping
      val visitedBefore = analysisWithUpdates.visited

      val beforeUpdateAnalysis = System.nanoTime
      analysisWithUpdates.updateAnalysis(timeout())
      val timeUpdateAnalysis = System.nanoTime - beforeUpdateAnalysis

      val storeWithUpdate = analysisWithUpdates.store
      val depsWithUpdate = analysisWithUpdates.deps
      val mappingWithUpdate = analysisWithUpdates.mapping
      val visitedWithUpdate = analysisWithUpdates.visited

      println("updating done")

      println("Time updating:                " + timeUpdateAnalysis)

      println("Store before: " + storeBefore.toString)
      println("Store with updating: " + storeWithUpdate.toString)

      println("Dependencies before: " + depsBefore.toString)
      println("Dependencies with updating: " + depsWithUpdate.toString)

      println("Mapping before: " + mappingBefore.toString)
      println("Mapping with updating: " + mappingWithUpdate.toString)

      println("Visited before: " + visitedBefore.toString)
      println("Visited with updating: " + visitedWithUpdate.toString)

    } catch {
      case e: Exception =>
        println(e)
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/symbols.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  println("Done")