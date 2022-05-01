package maf.cli.experiments.updating

import maf.cli.experiments.updating.UpdatingPerformance.AnalysisType
import maf.language.CScheme.CSchemeParserWithSplitter
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration
import maf.modular.incremental.IncrementalConfiguration.noOptimisations
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdateTwoVersions, SchemeModFSemanticsUpdate, UpdateIncrementalSchemeModFBigStepSemantics}
import maf.modular.scheme.modf.{SchemeModFNoSensitivity, StandardSchemeModFComponents}
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.{Table, Timeout}

import scala.concurrent.duration.{Duration, MINUTES}

object intraAnalysisCount extends App:
    var intraC = 0
    var intraCU = 0

    def AnalysisType(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsUpdate
        with LIFOWorklistAlgorithm[SchemeExp]
        with UpdateIncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeTypeDomain
        with IncrementalModAnalysisWithUpdateTwoVersions(newProgram)
        with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {   override def warn(msg: String): Unit = ()
        var configuration: IncrementalConfiguration = noOptimisations
        override def intraAnalysis(
                                      cmp: Component
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra  with IncrementalGlobalStoreIntraAnalysis {
            override def analyzeWithTimeout(timeout: Timeout.T): Unit =
                if version == Old then intraC += 1
                if version == New then intraCU += 1
                super.analyzeWithTimeout(timeout)
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
        var writeToFile = "benchOutput/UpdatingPerformance/intraAnalyses.csv"

        val initialOnly = AnalysisType(oldProgram, newProgram)
        initialOnly.analyzeWithTimeout(timeout())

        results = results.add(filename + " (" + dir + ")", "Initial Analysis", intraC)

        intraC = 0
        intraCU = 0

        val withUpdates = initialOnly.deepCopy()
        val withoutUpdates = initialOnly.deepCopy()

        withoutUpdates.updateAnalysis(timeout())
        results = results.add(filename + " (" + dir + ")", "Without updates", intraCU)

        intraC = 0
        intraCU = 0

        withUpdates.withUpdating = true
        withUpdates.updateAnalysis(timeout())
        results = results.add(filename + " (" + dir + ")", "With updates", intraCU)

        intraC = 0
        intraCU = 0

        val newOnly = AnalysisType(oldProgram, newProgram)
        newOnly.version = New
        newOnly.analyzeWithTimeout(timeout())
        results = results.add(filename + " (" + dir + ")", "New Analysis", intraCU)
        intraC = 0
        intraCU = 0


    val benchmarks: List[String] =
        List("test/changeDetectionTest/benchmarks/scope changes/nbody-processed.scm",
             "test/changeDetectionTest/benchmarks/ifs/nbody-processed.scm",
             "test/changeDetectionTest/benchmarks/renamings/nbody-processed.scm",
             "test/changeDetectionTest/benchmarks/scope changes/nboyer.scm",
             "test/changeDetectionTest/benchmarks/ifs/nboyer.scm",
             "test/changeDetectionTest/benchmarks/renamings/nboyer.scm",
             "test/changeDetectionTest/benchmarks/scope changes/peval.scm",
             "test/changeDetectionTest/benchmarks/ifs/peval.scm",
             "test/changeDetectionTest/benchmarks/renamings/peval.scm")

    benchmarks.foreach(file =>
        onBenchmark(file)
    )

    println(results.prettyString())
