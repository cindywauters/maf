package maf.cli.experiments.updating

import maf.cli.experiments.updating.UpdatingPerformance.AnalysisType
import maf.core
import maf.core.{Identifier, Monad, MonadError, MonadJoin}
import maf.language.CScheme.CSchemeParserWithSplitter
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration
import maf.modular.incremental.IncrementalConfiguration.noOptimisations
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdateTwoVersions, SchemeModFSemanticsUpdate, UpdateIncrementalSchemeModFBigStepSemantics}
import maf.modular.scheme.{PtrAddr, VarAddr}
import maf.modular.scheme.modf.{SchemeModFCallSiteSensitivity, SchemeModFComponent, SchemeModFNoSensitivity, StandardSchemeModFComponents}
import maf.modular.worklist.{FIFOWorklistAlgorithm, LIFOWorklistAlgorithm}
import maf.util.Reader
import maf.util.benchmarks.{Table, Timeout}

import java.io.{BufferedWriter, File, FileWriter}
import scala.concurrent.duration.{Duration, MINUTES}

object precisionPerformance extends App:

    def AnalysisType(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsUpdate
        with FIFOWorklistAlgorithm[SchemeExp]
        with UpdateIncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeTypeDomain
        with IncrementalModAnalysisWithUpdateTwoVersions(newProgram)
        with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {   override def warn(msg: String): Unit = ()
        var configuration: IncrementalConfiguration = noOptimisations
        override def intraAnalysis(
                                      cmp: Component
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra  with IncrementalGlobalStoreIntraAnalysis {
        }}

    var renamingBenchmarks = "test/changeDetectionTest/benchmarks/renamings"
    var scopeChangesBenchmarks = "test/changeDetectionTest/benchmarks/scope changes"
    var ifsBenchmarks = "test/changeDetectionTest/benchmarks/ifs"

    def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))

    var results: Table[Int] = Table.empty.withDefaultValue(0)

    def onBenchmark(file: String): Unit =
        val (oldProgram, newProgram) = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(file))
        val fullfilename = file.split("\\\\|\\.|\\/")
        var dir = ""
        var filename = ""
        if fullfilename != null && fullfilename.length > 4 then
            dir = fullfilename(3).asInstanceOf[String]
            filename = fullfilename(4).asInstanceOf[String]

        val initialOnly = AnalysisType(oldProgram, newProgram)
        initialOnly.analyzeWithTimeout(timeout())


        val withUpdates = initialOnly.deepCopy()
        val withoutUpdates = initialOnly.deepCopy()

        withoutUpdates.updateAnalysis(timeout())

        withUpdates.withUpdating = true
        withUpdates.updateAnalysis(timeout())

        println(withUpdates.store.size)
        println(withoutUpdates.store.size)

        var lessPrecise = 0
        var morePrecise = 0
        withUpdates.store.foreach((k, v) =>
            withoutUpdates.store.get(k) match
                case Some(updatedV) =>
                    if updatedV.asInstanceOf[withUpdates.Value] != v then
                        if withUpdates.lattice.subsumes(updatedV.asInstanceOf[withUpdates.Value], v) then
                            morePrecise += 1
                        else if withUpdates.lattice.subsumes(v, updatedV.asInstanceOf[withUpdates.Value]) then
                            lessPrecise += 1




        )

        results = results.add(dir + " " + filename + " store values", "with updates", withUpdates.store.size)
        results = results.add(dir + " " + filename +" store values", "without updates", withoutUpdates.store.size)
        results = results.add(dir + " " + filename + " store values (more)", "with updates", morePrecise)
        results = results.add(dir + " " + filename +" store values (less)", "without updates", lessPrecise)
        results = results.add(dir + " " + filename + " visited", "with updates", withUpdates.visited.size)
        results = results.add(dir + " " + filename +" visited", "without updates", withoutUpdates.visited.size)
        results = results.add(dir + " " + filename + " deps", "with updates", withUpdates.deps.size)
        results = results.add(dir + " " + filename +" deps", "without updates", withoutUpdates.deps.size)
        results = results.add(dir + " " + filename + " mapping", "with updates", withUpdates.mapping.size)
        results = results.add(dir + " " + filename +" mapping", "without updates", withoutUpdates.mapping.size)
        println(results.prettyString())


    val benchmarks: List[String] =
        List("test/changeDetectionTest/benchmarks/scope changes/nbody-processed.scm",
            "test/changeDetectionTest/benchmarks/ifs/nbody-processed.scm",
            "test/changeDetectionTest/benchmarks/renamings/nbody-processed.scm",
            "test/changeDetectionTest/benchmarks/scope changes/nboyer.scm",
            "test/changeDetectionTest/benchmarks/ifs/nboyer.scm",
            "test/changeDetectionTest/benchmarks/renamings/nboyer.scm",
            "test/changeDetectionTest/benchmarks/scope changes/peval.scm",
            "test/changeDetectionTest/benchmarks/ifs/peval.scm",
            "test/changeDetectionTest/benchmarks/renamings/peval.scm",
            "test/changeDetectionTest/benchmarks/scope changes/mceval.scm",
            "test/changeDetectionTest/benchmarks/ifs/mceval.scm",
            "test/changeDetectionTest/benchmarks/renamings/mceval.scm",
            "test/changeDetectionTest/benchmarks/scope changes/browse.scm",
            "test/changeDetectionTest/benchmarks/ifs/browse.scm",
            "test/changeDetectionTest/benchmarks/renamings/browse.scm",
            "test/changeDetectionTest/benchmarks/scope changes/freeze.scm",
            "test/changeDetectionTest/benchmarks/ifs/freeze.scm",
            "test/changeDetectionTest/benchmarks/renamings/freeze.scm",
            "test/changeDetectionTest/benchmarks/scope changes/matrix.scm",
            "test/changeDetectionTest/benchmarks/ifs/matrix.scm",
            "test/changeDetectionTest/benchmarks/renamings/matrix.scm",
            "test/changeDetectionTest/benchmarks/scope changes/leval.scm",
            "test/changeDetectionTest/benchmarks/ifs/leval.scm",
            "test/changeDetectionTest/benchmarks/renamings/leval.scm",
            "test/changeDetectionTest/benchmarks/scope changes/multiple-dwelling.scm",
            "test/changeDetectionTest/benchmarks/ifs/multiple-dwelling.scm",
            "test/changeDetectionTest/benchmarks/renamings/multiple-dwelling.scm",
            "test/changeDetectionTest/benchmarks/scope changes/machine-simulator.scm",
            "test/changeDetectionTest/benchmarks/ifs/machine-simulator.scm",
            "test/changeDetectionTest/benchmarks/renamings/machine-simulator.scm"
        )

    benchmarks.foreach(file =>
        onBenchmark(file)
    )

    println(results.prettyString())
    var writeToFile = "benchOutput/UpdatingPerformance/precision.csv"
    val outFile = new File(writeToFile)
    val bw = new BufferedWriter(new FileWriter(outFile))
    bw.write(results.toCSVString())
    bw.close()
