package maf.test.modular.scheme.updatingStructures
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.{BasicEnvironment, Expression}
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
import maf.modular.scheme.modf
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

  def checkSubsumptionContexts(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp], ac: Any, uc: Any): Boolean =
    a match
      case a: IncrementalGlobalStore[SchemeExp] =>
        (ac, uc) match
          case (ac: maf.modular.scheme.modf.ArgCallSiteContext, uc: maf.modular.scheme.modf.ArgCallSiteContext) =>
            ac.args.forall(v =>
              uc.args.exists(w =>
                a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
          case (ac: maf.modular.scheme.modf.ArgContext, uc: maf.modular.scheme.modf.ArgContext)  =>
            ac.values.forall(v =>
              uc.values.exists(w => a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
          case (Some(ac: maf.modular.scheme.modf.ArgCallSiteContext), Some(uc: maf.modular.scheme.modf.ArgCallSiteContext)) =>
            ac.args.forall(v =>
              uc.args.exists(w =>
                a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
          case (Some(ac: maf.modular.scheme.modf.ArgContext), Some(uc: maf.modular.scheme.modf.ArgContext))  =>
            ac.values.forall(v =>
              uc.values.exists(w => a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
          case (ac: _, uc: _) =>
            ac.equals(uc)


  def checkSubsumptionOneComponent(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp], ac: a.Component, uc: u.Component): Boolean =
    a match
      case a: IncrementalGlobalStore[SchemeExp] =>
        (ac, uc) match
          case (SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _), SchemeModFComponent.Call((wlam: SchemeLambdaExp, wenv: BasicEnvironment[_]), woldCtx: _)) =>
            if lam.equals(wlam) && wenv.equals(wenv) then
              checkSubsumptionContexts(a, u, oldCtx, woldCtx)
            else false
          case (ac: _, uc: _) => ac.equals(uc)
      case _ => false

  def checkSubsumptionSetOfComponents(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp], ac: Set[a.Component], uc: Set[u.Component]): Boolean =
    a match
      case a: IncrementalGlobalStore[SchemeExp] =>
        ac.forall(av =>
          uc.exists(uv =>
            checkSubsumptionOneComponent(a, u, av.asInstanceOf[a.Component], uv.asInstanceOf[u.Component])))
      case _ => false

  def checkSubsumptionSetOfDependencies(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp]): Boolean =
    a match
      case a: IncrementalGlobalStore[SchemeExp] =>
        var ac = a.deps
        val uc = u.deps
        ac.forall(av =>
          uc.exists(uv =>
            (av._1, uv._1) match
              case (k1: AddrDependency, k2: AddrDependency) =>
                (k1.addr, k2.addr) match
                  case (addr1: maf.modular.scheme.VarAddr[_], addr2: maf.modular.scheme.VarAddr[_]) =>
                    if addr1.equals(addr2) then
                      true
                    else
                      addr1.idn.equals(addr2.idn) && addr1.id.equals(addr2.id) && checkSubsumptionContexts(a, u, addr1.ctx, addr2.ctx) && checkSubsumptionSetOfComponents(a, u, av._2, uv._2)
                  case (addr1: maf.modular.ReturnAddr[_], addr2: maf.modular.ReturnAddr[_]) =>
                    if addr1.equals(addr2) then
                      true
                    else
                      addr1.idn.equals(addr2.idn) && checkSubsumptionOneComponent(a, u, addr1.cmp.asInstanceOf[a.Component], addr2.cmp.asInstanceOf[u.Component]) && checkSubsumptionSetOfComponents(a, u, av._2, uv._2)
                  case (addr1: maf.modular.scheme.PtrAddr[_], addr2: maf.modular.scheme.PtrAddr[_]) =>
                    if addr1.equals(addr2) then
                      true
                    else addr1.idn.equals(addr2.idn) && checkSubsumptionContexts(a, u, addr1.ctx, addr2.ctx) && checkSubsumptionSetOfComponents(a, u, av._2, uv._2)
                  case (addr1: _, addr2: _) =>
                    addr1.equals(addr2) && checkSubsumptionSetOfComponents(a, u, av._2, uv._2)
              case _ => false))

  def checkSubsumptionSetOfStore(a: IncrementalGlobalStore[SchemeExp], u: IncrementalGlobalStoreWithUpdate[SchemeExp]): Boolean =
    val ac = a.store
    val uc = u.store
    ac.forall(av =>
      uc.exists(uv =>
        (av._1, uv._1) match
          case (addr1: maf.modular.scheme.VarAddr[_], addr2: maf.modular.scheme.VarAddr[_]) =>
            if addr1.equals(addr2) then
              true
            else addr1.idn.equals(addr2.idn) && addr1.id.equals(addr2.id) && checkSubsumptionContexts(a, u, addr1.ctx, addr2.ctx) && a.lattice.subsumes(uv._2.asInstanceOf[a.Value], av._2)
          case (addr1: maf.modular.ReturnAddr[_], addr2: maf.modular.ReturnAddr[_]) =>
            if addr1.equals(addr2) then
              true
            else addr1.idn.equals(addr2.idn) && checkSubsumptionOneComponent(a, u, addr1.cmp.asInstanceOf[a.Component], addr2.cmp.asInstanceOf[u.Component]) && a.lattice.subsumes(uv._2.asInstanceOf[a.Value], av._2)
          case (addr1: maf.modular.scheme.PtrAddr[_], addr2: maf.modular.scheme.PtrAddr[_]) =>
            if addr1.equals(addr2) then
              true
            else addr1.idn.equals(addr2.idn) && checkSubsumptionContexts(a, u, addr1.ctx, addr2.ctx) && a.lattice.subsumes(uv._2.asInstanceOf[a.Value], av._2)
          case (addr1: _, addr2: _) =>
            addr1.equals(addr2) &&  a.lattice.subsumes(uv._2.asInstanceOf[a.Value], av._2)
          case _ => false
      ))

  def checkSubsumption(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp], full: IncrementalModAnalysis[SchemeExp]): Unit =
    // Check components.
    assert(a.visited.size <= u.visited.size, "The visited set of the incremental analysis update has less components than the analysis of new only.")
    assert(u.visited.size <= full.visited.size, "The visited set of the full incremental analysis update has less components than the incremental analysis with updating.")
    // Remove all visited that are exactly the same
    val adiffuVisited = a.visited.diff(u.visited.map(_.asInstanceOf[a.Component]))
    assert(checkSubsumptionSetOfComponents(a, u, adiffuVisited, u.visited), "The visited set of the incremental update does not subsume the visited set of the incremental analysis with updates.") // If the size is equal, this checks also the converse assertion.

    // Check dependencies.
    val depsA = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
    val depsU = u.deps.toSet[(Dependency, Set[u.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
    assert(depsA.size <= depsU.size, "The incremental analysis update has more dependencies than the regular incremental analysis.")
    val adiffuDeps = depsA.diff(depsU.map(e => (e._1, e._2.asInstanceOf[a.Component])))
    assert(checkSubsumptionSetOfDependencies(a, u), "The dependencies of the incremental update does not subsume the dependencies of the incremental analysis with updates.")

    // Check store.
    (a, u, full) match
      case (a: IncrementalGlobalStore[SchemeExp], u: IncrementalGlobalStoreWithUpdate[SchemeExp], full: IncrementalGlobalStore[SchemeExp]) =>
        assert(a.store.size <= u.store.size, "The incrementally updated store is smaller than the store of the incremental reanalysis with updates.")
        assert(checkSubsumptionSetOfStore(a, u), "The store of incremental update does not subsume the store of the analysis of the new version only")
      /*  a.store.foreach { case (addr, av) =>
          val uv = u.store.getOrElse(addr, u.lattice.bottom)
          val fullv = full.store.getOrElse(addr, full.lattice.bottom)
          val aSubsumesU = a.lattice.subsumes(uv.asInstanceOf[a.Value],av)
          assert(checkSubsumptionSetOfStore(a, u), s"Store mismatch at $addr: $uv is not subsumed by $av.")
     }*/


  val gambitGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit")()

  val modFbenchmarks: Set[String] = gambitGenerated

  modFbenchmarks.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
      property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysis(program), NoSensitivityAnalysisUpdate(program), NoSensitivityAnalysis(program), program)
    }
      property(s"Full Arg sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgSensitivityAnalysis(program), FullArgSensitivityAnalysisUpdate(program), FullArgSensitivityAnalysis(program), program)
    }
      property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysis(program), CallSensitivityAnalysisUpdate(program), CallSensitivityAnalysis(program), program)
    }
      property(s"Full Arg Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(FullArgCallSensitivityAnalysis(program), FullArgCallSensitivityAnalysisUpdate(program), FullArgCallSensitivityAnalysis(program), program)
    }
  )


  val gambitGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit/NoSensitivity")()

  val onlyCallSensitivity = gambitGeneratedContextInsensitive

  onlyCallSensitivity.foreach(benchmark =>
    val program = CSchemeParser.parseProgram(Reader.loadFile(benchmark))
      property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(NoSensitivityAnalysis(program), NoSensitivityAnalysisUpdate(program), NoSensitivityAnalysis(program), program)
    }
      property(s"Call sensitivity: Check if datastructures are the same in the analysis of new version and update for" + benchmark) {
      callAnalysisOnBenchmark(CallSensitivityAnalysis(program), CallSensitivityAnalysisUpdate(program), CallSensitivityAnalysis(program), program)
    }

  )

  def callAnalysisOnBenchmark(a: IncrementalModAnalysis[SchemeExp], u: IncrementalModAnalysisWithUpdate[SchemeExp], full:  IncrementalModAnalysis[SchemeExp], program: SchemeExp): Unit =
    u.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))
    u.updateAnalysis(Timeout.start(Duration(2, MINUTES)))

    a.version = New
    a.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))

    //a.updateAnalysis(Timeout.start(Duration(2, MINUTES)))

    full.analyzeWithTimeout(Timeout.start(Duration(2, MINUTES)))
    full.updateAnalysis(Timeout.start(Duration(2, MINUTES)))

    checkSubsumption(a, u, full)
