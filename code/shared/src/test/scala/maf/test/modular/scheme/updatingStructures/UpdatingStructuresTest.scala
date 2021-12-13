package maf.test.modular.scheme.updatingStructures
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.{IncrementalModAnalysis, *}
import maf.modular.incremental.scheme.lattice.{IncrementalSchemeConstantPropagationDomain, IncrementalSchemeTypeDomain}
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.modf.*
import maf.modular.scheme.ssmodconc.*
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.test.*
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag
import org.scalatest.propspec.AnyPropSpec

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
    assert(updated.visited == analysisOfNew.visited, "The visited sets of the updated program differs from the visited set of the analysis of the new program.")
    assert(checkDeps(updated, analysisOfNew), "The dependencies of the updated program differs from the dependencies of the analysis of the new program.")
    assert(checkMapping(updated, analysisOfNew), "The dependencies of the updated program differs from the dependencies of the analysis of the new program.")

    (updated, analysisOfNew) match
      case (updated: IncrementalGlobalStore[SchemeExp], analysisOfNew: IncrementalGlobalStore[SchemeExp]) =>
        assert(checkStores(updated, analysisOfNew), "The store of the updated program differs from the store of the analysis of the new program.")
      case _ =>

 // val analyses: List[IncrementalModAnalysis[SchemeExp]] = List(NoSensitivityAnalysis, FullArgSensitivityAnalysis, CallSensitivityAnalysis, FullArgCallSensitivityAnalysis)
  val modFbenchmarks: List[String] = List(
    "test/changeDetectionTest/onlyConsistentRenaming/smallLambdaTests.scm",
    "test/changeDetectionTest/onlyConsistentRenaming/moreLambdas.scm",
    "test/changeDetectionTest/onlyConsistentRenaming/moreLambdas2.scm",
    "test/changeDetectionTest/onlyConsistentRenaming/Vectors.scm",
    "test/changeDetectionTest/onlyConsistentRenaming/Lists.scm")

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
//    callAnalysisOnBenchmark(FullArgSensitivityAnalysis(program), program)
//    callAnalysisOnBenchmark(CallSensitivityAnalysis(program), program)
//    callAnalysisOnBenchmark(FullArgCallSensitivityAnalysis(program), program)
  )

  def callAnalysisOnBenchmark(a: IncrementalModAnalysis[SchemeExp], program: SchemeExp): Unit =
    val analysisToUpdate = a.deepCopy()
    analysisToUpdate.analyzeWithTimeoutInSeconds(60)
    var update = new IncrementalUpdateDatastructures
    update.changeDataStructures(analysisToUpdate, program)

    val analysisNew = a.deepCopy()
    analysisNew.version = New
    analysisNew.analyzeWithTimeoutInSeconds(60)

    checkEqual(analysisToUpdate, analysisNew)







def checkEqualVisited(updated: IncrementalModAnalysis[SchemeExp], analysisOfNew: IncrementalModAnalysis[SchemeExp]): Boolean =
  updated.visited.forall(e =>
    analysisOfNew.visited.contains(e.asInstanceOf[analysisOfNew.Component]))
  &&
  analysisOfNew.visited.forall(e =>
    updated.visited.contains(e.asInstanceOf[updated.Component]))

def checkDeps(updated: IncrementalModAnalysis[SchemeExp], analysisOfNew: IncrementalModAnalysis[SchemeExp]): Boolean =
  updated.deps.forall((k, v) =>
    analysisOfNew.deps.get(k) match
      case Some(updatedValue) => updatedValue.==(v)
      case _ => false)
    &&
    analysisOfNew.deps.forall((k, v) =>
      updated.deps.get(k) match
        case Some(updatedValue) => updatedValue.==(v)
        case _ => false)

def checkMapping(updated: IncrementalModAnalysis[SchemeExp], analysisOfNew: IncrementalModAnalysis[SchemeExp]): Boolean =
  updated.mapping.forall((k, v) =>
    analysisOfNew.mapping.get(k) match
      case Some(updatedValue) => updatedValue.==(v)
      case _ => false)
    &&
    analysisOfNew.mapping.forall((k, v) =>
      updated.mapping.get(k) match
        case Some(updatedValue) => updatedValue.==(v)
        case _ => false)

def checkStores(updated: IncrementalGlobalStore[SchemeExp], analysisOfNew: IncrementalGlobalStore[SchemeExp]): Boolean =
  var allSame = true
  updated.store.foreach((k, v) =>
    analysisOfNew.store.get(k) match
      case Some(updatedValue) => true
      case _ => false)
  analysisOfNew.store.forall((k, v) =>
    updated.store.get(k) match
      case Some(updatedValue) => updatedValue.==(v)
      case _ => false)

