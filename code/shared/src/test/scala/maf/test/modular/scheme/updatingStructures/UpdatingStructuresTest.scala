package maf.test.modular.scheme.updatingStructures
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Expression
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.{IncrementalModAnalysis, *}
import maf.modular.incremental.scheme.lattice.{IncrementalSchemeConstantPropagationDomain, IncrementalSchemeTypeDomain}
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.incremental.update.IncrementalUpdateDatastructures
import maf.modular.scheme.modf.*
import maf.modular.scheme.ssmodconc.*
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test.*
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag
import org.scalatest.propspec.AnyPropSpec

import scala.concurrent.duration.{Duration, MINUTES}

class UpdatingStructuresTest extends AnyPropSpec:

  type Analysis <: IncrementalModAnalysis[SchemeExp]

  abstract class BaseAnalysis(program: SchemeExp)
    extends ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain

  class NoSensitivityAnalysis(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFNoSensitivity
      with IncrementalGlobalStore[SchemeExp]:
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

  class FullArgSensitivityAnalysis(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFFullArgumentSensitivity
      with IncrementalGlobalStore[SchemeExp]:
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

  class CallSensitivityAnalysis(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFCallSiteSensitivity
      with IncrementalGlobalStore[SchemeExp]:
      var configuration: IncrementalConfiguration =  noOptimisations
      override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

  class FullArgCallSensitivityAnalysis(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFFullArgumentCallSiteSensitivity
      with IncrementalGlobalStore[SchemeExp]:
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis


  def checkEqual(updated: IncrementalModAnalysis[SchemeExp], analysisOfNew: IncrementalModAnalysis[SchemeExp]): Unit =
    assert(updated.visited.hashCode() == analysisOfNew.visited.hashCode(), "The visited sets of the updated program differs from the visited set of the analysis of the new program.")
    assert(updated.deps.hashCode() == analysisOfNew.deps.hashCode(), "The dependencies of the updated program differs from the dependencies of the analysis of the new program.")
    assert(updated.mapping.hashCode() == analysisOfNew.mapping.hashCode(), "The mappings of the updated program differs from the mappings of the analysis of the new program.")

    (updated, analysisOfNew) match
      case (updated: IncrementalGlobalStore[SchemeExp], analysisOfNew: IncrementalGlobalStore[SchemeExp]) =>
        assert(updated.store.hashCode() == analysisOfNew.store.hashCode() , "The store of the updated program differs from the store of the analysis of the new program.")
      case _ =>

  val firstTests: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming")()

  val gambitGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/gambit")()

  val adGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/ad")()

  val tlsGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/WeiChenRompf2019/the-little-schemer")()

  val scp1Generated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/scp1")()

  val scp1CompressedGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/scp1-compressed")()

  val variousGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/various")()

  val modFbenchmarks: Set[String] = variousGenerated ++ scp1CompressedGenerated ++ scp1Generated ++ tlsGenerated ++ firstTests ++ gambitGenerated ++ adGenerated

  modFbenchmarks.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
    property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysis(program), program)
    }
    property(s"Full Arg sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgSensitivityAnalysis(program), program)
    }
    property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysis(program), program)
    }
      property(s"Full Arg Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgCallSensitivityAnalysis(program), program)
    }
  )


  val gambitGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/gambit/NoSensitivity")()

  val adGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/ad/NoSensitivity")()

  val tlsGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/WeiChenRompf2019/the-little-schemer/NoSensitivity")()

  val scp1GeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/scp1/NoSensitivity")()

  val scp1ComGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/scp1-compressed/NoSensitivity")()

  val VariousGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/onlyConsistentRenaming/R5RS/various/NoSensitivity")()

  val onlyCallSensitivity = tlsGeneratedContextInsensitive ++ gambitGeneratedContextInsensitive ++ adGeneratedContextInsensitive ++ scp1GeneratedContextInsensitive ++ scp1ComGeneratedContextInsensitive ++ VariousGeneratedContextInsensitive

  onlyCallSensitivity.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
      property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysis(program), program)
    }
      property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysis(program), program)
    }

  )

  def callAnalysisOnBenchmark(a: IncrementalModAnalysis[SchemeExp], program: SchemeExp): Unit =
    val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))
    val analysisToUpdate = a.deepCopy()
    analysisToUpdate.analyzeWithTimeout(standardTimeout())
    var update = new IncrementalUpdateDatastructures
    analysisToUpdate match
      case analysis: IncrementalModAnalysis[Expression] =>
        val changedAndRenamings = SchemeChangePatterns.checkForRenamingParameter(program).filter(e => e._2._1).map(e => (e._1, e._2._2))
        update.changeDataStructures(analysis, program, changedAndRenamings)

    val analysisNew = a.deepCopy()
    analysisNew.version = New
    analysisNew.analyzeWithTimeout(standardTimeout())

    checkEqual(analysisToUpdate, analysisNew)
