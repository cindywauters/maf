package maf.test.modular.scheme.updatingStructures

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.New
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration.noOptimisations
import maf.modular.incremental.{IncrementalConfiguration, IncrementalGlobalStore, IncrementalModAnalysis}
import maf.modular.incremental.scheme.lattice.{IncrementalSchemeConstantPropagationDomain, IncrementalSchemeTypeDomain}
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdate, IncrementalUpdateDatastructures}
import maf.modular.scheme.modf.*
import maf.modular.scheme.ssmodconc.*
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test.*
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag
import org.scalatest.propspec.AnyPropSpec

import scala.concurrent.duration.{Duration, MINUTES}

class IncrementalAnalysisUpdateNoRenamingsTest extends AnyPropSpec:

  type Analysis <: IncrementalModAnalysis[SchemeExp]

  abstract class BaseAnalysis(program: SchemeExp)
    extends ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain

  class NoSensitivityAnalysisUpdate(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFNoSensitivity
      with IncrementalGlobalStoreWithUpdate[SchemeExp]:
    var configuration: IncrementalConfiguration = noOptimisations
    override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

  class FullArgSensitivityAnalysisUpdate(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFFullArgumentSensitivity
      with IncrementalGlobalStoreWithUpdate[SchemeExp]:
    var configuration: IncrementalConfiguration = noOptimisations
    override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

  class CallSensitivityAnalysisUpdate(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFCallSiteSensitivity
      with IncrementalGlobalStoreWithUpdate[SchemeExp]:
    var configuration: IncrementalConfiguration =  noOptimisations
    override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

  class FullArgCallSensitivityAnalysisUpdate(program: SchemeExp)
    extends BaseAnalysis(program)
      with SchemeModFFullArgumentCallSiteSensitivity
      with IncrementalGlobalStoreWithUpdate[SchemeExp]:
    var configuration: IncrementalConfiguration = noOptimisations
    override def intraAnalysis(
                                cmp: Component
                              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis

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


  val skips = List(
    "test\\changes\\scheme\\multiple-dwelling (coarse).scm",
    "test\\changes\\scheme\\multiple-dwelling (fine).scm",
    "test\\changes\\scheme\\primtest.scm",
    "test\\changes\\scheme\\peval.scm",
    "test\\changes\\scheme\\machine-simulator.scm",
    "test\\changes\\scheme\\nboyer.scm",
    "test\\changes\\scheme\\scheme.scm",
    "test\\changes\\scheme\\mceval-dynamic.scm"
  )
  val modFbenchmarks: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")().filter(file => !skips.contains(file))

  modFbenchmarks.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
      property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysisUpdate(program), NoSensitivityAnalysis(program), program)
    }
    /*  property(s"Full Arg sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysisUpdate(program), NoSensitivityAnalysis(program), program)
    }*/
      property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysisUpdate(program), CallSensitivityAnalysis(program), program)
    }
 /*     property(s"Full Arg Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgCallSensitivityAnalysisUpdate(program), FullArgCallSensitivityAnalysis(program), program)
    }*/
  )

  def callAnalysisOnBenchmark(analysisToUpdate: IncrementalModAnalysisWithUpdate[SchemeExp], analysisIncremental: IncrementalModAnalysis[SchemeExp], program: SchemeExp): Unit =
    analysisToUpdate.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))
    analysisToUpdate.updateAnalysis(Timeout.start(Duration(2, MINUTES)))

    analysisIncremental.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))
    analysisIncremental.updateAnalysis(Timeout.start(Duration(2, MINUTES)))

    checkEqual(analysisToUpdate, analysisIncremental)

