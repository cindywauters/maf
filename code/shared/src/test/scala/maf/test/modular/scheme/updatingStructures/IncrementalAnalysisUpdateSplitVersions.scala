package maf.test.modular.scheme.updatingStructures

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.{BasicEnvironment, Expression}
import maf.language.CScheme.{CSchemeParser, CSchemeParserWithSplitter}
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.modular.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.{IncrementalModAnalysis, *}
import maf.modular.incremental.scheme.lattice.{IncrementalSchemeConstantPropagationDomain, IncrementalSchemeTypeDomain}
import maf.modular.incremental.scheme.modconc.IncrementalSchemeModConcSmallStepSemantics
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdate, IncrementalModAnalysisWithUpdateTwoVersions, IncrementalUpdateDatastructures, SchemeModFSemanticsUpdate, UpdateIncrementalSchemeModFBigStepSemantics}
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

class IncrementalAnalysisUpdateSplitVersions extends AnyPropSpec:

    type Analysis <: IncrementalModAnalysis[SchemeExp]
    type AnalysisWithUpdate <: IncrementalModAnalysisWithUpdate[SchemeExp]

    def baseAnalysisUpdateInsertDelete(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
        with StandardSchemeModFComponents
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
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis    }

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

    def checkSubsumptionForUpdate(nwOnly: IncrementalModAnalysisWithUpdate[SchemeExp], updates: IncrementalModAnalysisWithUpdate[SchemeExp]): Unit =
        // Check components.
        assert(nwOnly.visited.size <= updates.visited.size, "The visited set of the incremental analysis update has less components than the analysis of new only.")
        // Remove all visited that are exactly the same
        val nwDiffupdatesVisited = nwOnly.visited.diff(updates.visited.map(_.asInstanceOf[nwOnly.Component]))
        assert(checkSubsumptionSetOfComponents(nwOnly, updates, nwDiffupdatesVisited, updates.visited), "The visited set of the incremental update does not subsume the visited set of the incremental analysis with updates.") // If the size is equal, this checks also the converse assertion.

        // Check dependencies.
        val depsNw = nwOnly.deps.toSet[(Dependency, Set[nwOnly.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        val depsU = updates.deps.toSet[(Dependency, Set[updates.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        assert(depsNw.size <= depsU.size, "The incremental analysis update has more dependencies than the regular incremental analysis.")
        assert(checkSubsumptionSetOfDependencies(nwOnly, updates), "The dependencies of the incremental update does not subsume the dependencies of the incremental analysis with updates.")

        val mappingsNw = nwOnly.mapping
        val mappingsUpdate = updates.mapping
        assert(mappingsNw.size <= mappingsUpdate.size, "The incremental analysis update has more mappings than the regular incremental analysis.")
        assert(mappingsNw.forall((k, v) =>
            mappingsUpdate.get(k) match
                case Some(updatedValue) =>
                    v.forall(elv => updatedValue.contains(elv.asInstanceOf[updates.Component]))
                case _ =>
                    false), "The mappings of incremental update does not subsume the mappings of the analysis of the new version only")

        // Check store.
        (nwOnly, updates) match
            case (a: IncrementalGlobalStore[SchemeExp], u: IncrementalGlobalStoreWithUpdate[SchemeExp]) =>
                assert(a.store.size <= u.store.size, "The incrementally updated store is smaller than the store of the incremental reanalysis with updates.")
                assert(checkSubsumptionSetOfStore(a, u), "The store of incremental update does not subsume the store of the analysis of the new version only")


    val gambitGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit")()
    val gambitGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit/NoSensitivity")()

    val others: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")()
    val manualScopeChanges: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/scopeChangesManual")()

    val benchmarksScopes: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/scope changes")()
    val benchmarksIfs: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/ifs")()
    val benchmarksNames: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/renamings")()

    val modFbenchmarks: Set[String] = gambitGenerated ++ gambitGeneratedContextInsensitive ++ others ++ manualScopeChanges ++ benchmarksScopes ++ benchmarksIfs ++ benchmarksNames

    modFbenchmarks.foreach(benchmark =>
        val twoPrograms = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(benchmark))
            property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for " + benchmark) {
            callAnalysisOnBenchmark(benchmark, baseAnalysisUpdateInsertDelete(twoPrograms._1, twoPrograms._2), baseAnalysisUpdateInsertDelete(twoPrograms._1, twoPrograms._2), baseAnalysisUpdateInsertDelete(twoPrograms._1, twoPrograms._2))}
    )

    def callAnalysisOnBenchmark(benchmark: String, newOnly: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp], withUpdates: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp], withoutUpdates: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]): Unit =
        val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

        newOnly.version = New
        newOnly.analyzeWithTimeout(standardTimeout())

        withUpdates.analyzeWithTimeout(standardTimeout())
        withUpdates.version = New
        withUpdates.withUpdating = true
        withUpdates.updateAnalysis(standardTimeout())

        withoutUpdates.analyzeWithTimeout(standardTimeout())
        withoutUpdates.version = New
        withUpdates.withUpdating = false
        withoutUpdates.updateAnalysis(standardTimeout())

        checkSubsumptionForUpdate(newOnly, withUpdates)
        checkSubsumptionForUpdate(newOnly, withoutUpdates)
// checkSubsumptionForUpdate(twoVersionsWithUpdate, twoVersions)

