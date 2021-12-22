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

class IncrementalAnalysisUpdateMixOfChangesTest extends AnyPropSpec:

  type Analysis <: IncrementalModAnalysis[SchemeExp]
  type AnalysisWithUpdate <: IncrementalModAnalysisWithUpdate[SchemeExp]

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

  def checkSubsumption(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp]): Unit =
    // Check components.
    assert(a.visited.size <= u.visited.size, "The visited set of the incremental analysis update has less components than the analysis of new only.")
    assert(a.visited.diff(u.visited.map(_.asInstanceOf[a.Component])).isEmpty, "The visited set of the incremental update does not subsume the visited set of the incremental analysis with updates.") // If the size is equal, this checks also the converse assertion.

    // Check dependencies.
    val depsA = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
    val depsU = u.deps.toSet[(Dependency, Set[u.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
    assert(depsA.size <= depsU.size, "The incremental analysis update has more dependencies than the regular incremental analysis.")
    assert(depsA.diff(depsU.map(e => (e._1, e._2.asInstanceOf[a.Component]))).isEmpty, "The dependencies of the incremental update does not subsume the dependencies of the incremental analysis with updates.")

    // Check store.
    (a, u) match
      case (a: IncrementalGlobalStore[SchemeExp], u: IncrementalGlobalStoreWithUpdate[SchemeExp]) =>
        assert(a.store.size <= u.store.size, "The incrementally updated store is smaller than the store of the incremental reanalysis with updates.")
        a.store.foreach { case (addr, av) =>
          val uv = u.store.getOrElse(addr, u.lattice.bottom)
          assert(a.lattice.subsumes(uv.asInstanceOf[a.Value],av), s"Store mismatch at $addr: $uv is not subsumed by $av.")
     }


  val gambitGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit")()

  val modFbenchmarks: Set[String] = gambitGenerated

  modFbenchmarks.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
      property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysis(program), NoSensitivityAnalysisUpdate(program), program)
    }
      property(s"Full Arg sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgSensitivityAnalysis(program), FullArgSensitivityAnalysisUpdate(program), program)
    }
      property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysis(program), CallSensitivityAnalysisUpdate(program), program)
    }
      property(s"Full Arg Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgCallSensitivityAnalysis(program), FullArgCallSensitivityAnalysisUpdate(program), program)
    }
  )


  val gambitGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit/NoSensitivity")()

  val onlyCallSensitivity = gambitGeneratedContextInsensitive

  onlyCallSensitivity.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
      property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysis(program), NoSensitivityAnalysisUpdate(program), program)
    }
      property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysis(program), CallSensitivityAnalysisUpdate(program), program)
    }

  )

  def callAnalysisOnBenchmark(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp], program: SchemeExp): Unit =
    u.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))
    u.updateAnalysis(Timeout.start(Duration(2, MINUTES)))

    a.version = New
    a.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))

    checkSubsumption(a, u)
