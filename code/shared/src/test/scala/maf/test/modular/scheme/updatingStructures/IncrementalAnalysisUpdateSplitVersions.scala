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
import maf.modular.worklist.{FIFOWorklistAlgorithm, LIFOWorklistAlgorithm}
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
        with FIFOWorklistAlgorithm[SchemeExp]
        with UpdateIncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeTypeDomain
        with IncrementalModAnalysisWithUpdateTwoVersions(newProgram)
        with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {
        var configuration: IncrementalConfiguration = noOptimisations
        override def warn(msg: String): Unit = ()
        override def intraAnalysis(
                                      cmp: Component
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis    }


    def baseAnalysis(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
        with StandardSchemeModFComponents
        //with SchemeModFFullArgumentCallSiteSensitivity
       // with SchemeModFCallSiteSensitivity
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsM
        with FIFOWorklistAlgorithm[SchemeExp]
        with IncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeTypeDomain // IncrementalSchemeConstantPropagationDomain
        with IncrementalGlobalStore[SchemeExp]
        // with IncrementalLogging[SchemeExp]
        //with IncrementalDataFlowVisualisation[SchemeExp]
    {
        override def warn(msg: String): Unit = ()
        var configuration: IncrementalConfiguration = noOptimisations
        override def intraAnalysis(
                                      cmp: Component
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
        //with IncrementalVisualIntra
    }

    def checkSubsumptionForUpdate(nwOnly: IncrementalModAnalysis[SchemeExp], updates: IncrementalModAnalysis[SchemeExp]): Unit =
        // Check components.
        assert(nwOnly.visited.size <= updates.visited.size, "The visited set of the incremental analysis update has less components than the analysis of new only.")
        // Remove all visited that are exactly the same
        val nwDiffupdatesVisited = nwOnly.visited.diff(updates.visited.map(_.asInstanceOf[nwOnly.Component]))
        assert(nwDiffupdatesVisited.isEmpty, "The visited set of the incremental update does not subsume the visited set of the incremental analysis with updates.") // If the size is equal, this checks also the converse assertion.

        // Check dependencies.
        val depsNw = nwOnly.deps.toSet[(Dependency, Set[nwOnly.Component])]//.flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        val depsU = updates.deps.toSet[(Dependency, Set[updates.Component])]//.flatMap({ case (d, cmps) => cmps.map(c => (d, c)) })
        assert(depsNw.size <= depsU.size, "The incremental analysis update has more dependencies than the regular incremental analysis.")
        assert(depsNw.forall((k, v) =>
            depsU.find(e => e._1 == k) match
                case Some(updatedValue) =>
                    v.forall(elv => updatedValue._2.contains(elv.asInstanceOf[updates.Component]))
                case _ => false), "The dependencies of the incremental update does not subsume the dependencies of the incremental analysis with updates.")

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
                assert(a.store.forall((k, v) =>
                    u.store.get(k) match
                        case Some(updatedValue) =>  a.lattice.subsumes(updatedValue.asInstanceOf[a.Value], v)
                        case _ => false), "The store of incremental update does not subsume the store of the analysis of the new version only")


    val gambitGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit")()
    val gambitGeneratedContextInsensitive: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/MixOfChanges/R5RS/gambit/NoSensitivity")()

    val others: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")()
    val manualScopeChanges: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/scopeChangesManual")()

    val benchmarksScopes: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/scope changes")()
    val benchmarksIfs: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/ifs")()
    val benchmarksNames: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/renamings")()

    val modFbenchmarks: Set[String] = benchmarksScopes ++ manualScopeChanges ++ gambitGenerated ++ gambitGeneratedContextInsensitive ++ others ++ manualScopeChanges ++ benchmarksScopes ++ benchmarksIfs ++ benchmarksNames
    //val modFbenchmarks: Set[String] = benchmarksScopes ++ benchmarksNames


    modFbenchmarks.foreach(benchmark =>
        val twoPrograms = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(benchmark))
            property(s"No sensitivity: Check if datastructures are the same in the analysis of new version and update for " + benchmark) {
            callAnalysisOnBenchmark(benchmark, baseAnalysis(twoPrograms._2), baseAnalysisUpdateInsertDelete(twoPrograms._1, twoPrograms._2), baseAnalysisUpdateInsertDelete(twoPrograms._1, twoPrograms._2))}
    )

    def callAnalysisOnBenchmark(benchmark: String, newOnly: IncrementalModAnalysis[SchemeExp], withUpdates: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp], withoutUpdates: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]): Unit =
        val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(3, MINUTES))

        newOnly.version = New
        newOnly.analyzeWithTimeout(standardTimeout())

        withUpdates.analyzeWithTimeout(standardTimeout())
        withUpdates.version = New
        var without = withUpdates.deepCopy()
        withUpdates.withUpdating = true
        withUpdates.updateAnalysis(standardTimeout())
        checkSubsumptionForUpdate(newOnly, withUpdates)

       // without.analyzeWithTimeout(standardTimeout())
       // withoutUpdates.version = New
        without.withUpdating = false
        without.updateAnalysis(standardTimeout())


        checkSubsumptionForUpdate(newOnly, without)
// checkSubsumptionForUpdate(twoVersionsWithUpdate, twoVersions)

