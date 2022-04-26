package maf.cli.experiments.updating

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.runnables.GenerateConsistentRenamings.replaceInParsed
import maf.core.Label.DPC
import maf.language.CScheme.{CSchemeParser, CSchemeParserWithSplitter}
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.SchemeExp
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.primitives.SchemePrimitives
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration
import maf.modular.incremental.IncrementalConfiguration.noOptimisations
import maf.modular.incremental.scheme.lattice.{IncrementalSchemeConstantPropagationDomain, IncrementalSchemeTypeDomain}
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdateTwoVersions, SchemeModFSemanticsUpdate, UpdateIncrementalSchemeModFBigStepSemantics}
import maf.modular.scheme.modf.{SchemeModFComponent, SchemeModFNoSensitivity, StandardSchemeModFComponents}
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.Reader
import maf.util.benchmarks.{Table, Timeout, Timer}

import java.io.{BufferedWriter, File, FileWriter}
import scala.concurrent.duration.{Duration, MINUTES}

object UpdatingPerformance extends App:
    def AnalysisType(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsUpdate
        with LIFOWorklistAlgorithm[SchemeExp]
        with UpdateIncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeTypeDomain
        with IncrementalModAnalysisWithUpdateTwoVersions(newProgram)
        with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {
        override def warn(msg: String): Unit = ()
        var configuration: IncrementalConfiguration = noOptimisations
        override def intraAnalysis(
                                      cmp: Component
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra  with IncrementalGlobalStoreIntraAnalysis }

    def AnalysisConstantPropagation(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsUpdate
        with LIFOWorklistAlgorithm[SchemeExp]
        with UpdateIncrementalSchemeModFBigStepSemantics
        with IncrementalSchemeConstantPropagationDomain
        with IncrementalModAnalysisWithUpdateTwoVersions(newProgram)
        with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {
        override def warn(msg: String): Unit = ()
        var configuration: IncrementalConfiguration = noOptimisations
        override def intraAnalysis(
                                      cmp: Component
                                  ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra  with IncrementalGlobalStoreIntraAnalysis }



    var renamingBenchmarks = "test/changeDetectionTest/benchmarks/renamings"
    var scopeChangesBenchmarks = "test/changeDetectionTest/benchmarks/scope changes"
    var ifsBenchmarks = "test/changeDetectionTest/benchmarks/ifs"

    var warmup = 5
    var rounds = 15

    def timeout(): Timeout.T = Timeout.start(Duration(5, MINUTES))

    var resultsNoRefactoring: Table[Int] = Table.empty.withDefaultValue(0)
    var resultsWithRefactoring: Table[Int] = Table.empty.withDefaultValue(0)
    var resultsNonIncremental: Table[Int] = Table.empty.withDefaultValue(0)
    var resultAverages: Table[Int] = Table.empty.withDefaultValue(0)

    def addToIncrementalInitial(table: Table[Int], a: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp], index: String, totalTime: Double): Table[Int] =
        var outputTable = table.add(index, "total time", totalTime.toInt)
        outputTable = outputTable.add(index, "change detection", (a.timeFindingChanges / 1000000).toInt)
        outputTable = outputTable.add(index, "updating datastructures", (a.timeUpdatingStructures / 1000000).toInt)
        outputTable = outputTable.add(index, "incremental analysis" , (a.timeIncrementalReanalysis / 1000000).toInt)
        outputTable

    def addToNonIncrementalInitial(index: String, totalTime: Double): Unit =
        resultsNonIncremental = resultsNonIncremental.add(index, "Initial: total time", totalTime.toInt)
    def addToNonIncrementalNew(index: String, totalTime: Double): Unit =
        resultsNonIncremental = resultsNonIncremental.add(index, "New only: total time", totalTime.toInt)

    def warmUp(msg: String, block: Timeout.T => Unit): Unit =
        print(s"Warmup: $msg ")
        val timeOut = timeout()
        for w <- 1 to warmup do
            print(s"$w ")
            System.gc()
            block(timeOut)
            if timeOut.reached then
                println()
                return
            println()

    def runOneTime(analysis: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp], block: (Timeout.T, IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]) => Unit): Option[Double] =
        System.gc()
        val to = timeout()
        val time = Timer.timeOnly(block(to, analysis))
        if to.reached then None
        else Some(time.toDouble / 1000000) // Return time in ms

    def runBenchmarks(msg: String,
                      createAnalysis: () => IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp],
                      block: (Timeout.T, IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]) => Unit,
                      oldVersion: Boolean = false,
                      incremental: Boolean = false,
                      withRefactorings: Boolean = false
                     ): Option[List[Double]] =
        print(s"Measuring: $msg ")
        var times: List[Double] = List()
        var timesFinding: List[Int] = List()
        var timesUpdating: List[Int] = List()
        var timesIncremental: List[Int] = List()
        for i <- 1 to rounds do
            print(s"$i ")
            val analysis = createAnalysis()
            runOneTime(analysis, block) match
                case Some(t) =>
                    times = t :: times
                    if !incremental then
                        if oldVersion then
                            addToNonIncrementalInitial(i.toString, t)
                        else
                            addToNonIncrementalNew(i.toString, t)
                    else
                        if withRefactorings then
                            resultsWithRefactoring = addToIncrementalInitial(resultsWithRefactoring, analysis, i.toString, t)
                        else
                            resultsNoRefactoring = addToIncrementalInitial(resultsNoRefactoring, analysis, i.toString, t)
                        timesFinding     = (analysis.timeFindingChanges / 1000000).toInt :: timesFinding
                        timesUpdating    = (analysis.timeUpdatingStructures / 1000000).toInt :: timesUpdating
                        timesIncremental = (analysis.timeIncrementalReanalysis / 1000000).toInt :: timesIncremental
                case None =>
                    println(" timed out.")
                    return None
            println()
        if incremental then
            if withRefactorings then
                resultAverages = resultAverages.add("With refactorings",  "avg finding", timesFinding.sum / timesFinding.length)
                resultAverages = resultAverages.add("With refactorings", "avg incremental analysis", timesIncremental.sum / timesIncremental.length)
                resultAverages = resultAverages.add("With refactorings", "avg updating", timesUpdating.sum / timesUpdating.length)
                resultAverages = resultAverages.add("With refactorings", "avg full", (times.sum / times.length).toInt)
            else
                resultAverages = resultAverages.add("No refactorings", "avg finding", timesFinding.sum / timesFinding.length)
                resultAverages = resultAverages.add("No refactorings", "avg incremental analysis", timesIncremental.sum / timesIncremental.length)
                resultAverages = resultAverages.add("No refactorings", "avg updating", timesUpdating.sum / timesUpdating.length)
                resultAverages = resultAverages.add("No refactorings", "avg full", (times.sum / times.length).toInt)
        else
            if oldVersion then
                resultAverages = resultAverages.add("Intial analysis", "avg full", (times.sum / times.length).toInt)
            else
                resultAverages = resultAverages.add("New analysis", "avg full", (times.sum / times.length).toInt)
        Some(times)

    def onBenchmark(file: String): Unit =
        val (oldProgram, newProgram) = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(file))
        val fullfilename = file.split("\\\\|\\.")
        println(file)
        var writeToFile = ""
        if fullfilename != null then
            val dir = fullfilename(3)
            val filename = fullfilename(4)
            writeToFile = "benchOutput/UpdatingPerformance/" + dir + "/" + filename + ".csv"

        warmUp("initial analysis", timeout => {
            val initial = AnalysisType(oldProgram, newProgram)
            initial.analyzeWithTimeout(timeout)
        })
        runBenchmarks(
            "initial analysis",
            () => {
                val initial = AnalysisType(oldProgram, newProgram)
                initial},
            (timeout, analysis) => analysis.analyzeWithTimeout(timeout))

        warmUp("analysis new only", timeout => {
            val newOnly = AnalysisType(oldProgram, newProgram)
            newOnly.version = New
            newOnly.analyzeWithTimeout(timeout)
        })

        runBenchmarks(
            "new analysis",
            () => {
                val newOnly = AnalysisType(oldProgram, newProgram)
                newOnly.version = New
                newOnly
            },
            (timeout, analysis) => analysis.analyzeWithTimeout(timeout),
            oldVersion = true)


        val initialAnalysis = AnalysisType(oldProgram, newProgram)
        initialAnalysis.analyzeWithTimeout(timeout())

        warmUp("incremental analysis with refactoring updates", timeout => {
            val withUpdates = initialAnalysis.deepCopy()
            withUpdates.withUpdating = true
            withUpdates.updateAnalysis(timeout)
        })

        runBenchmarks(
            "incremental analysis with refactoring updates",
            () => {
                val withUpdates = initialAnalysis.deepCopy()
                withUpdates.withUpdating = true
                withUpdates
            },
            (timeout, analysis) => analysis.updateAnalysis(timeout),
            incremental = true,
            withRefactorings = true)

        warmUp("incremental analysis without refactoring updates", timeout => {
            val withUpdates = initialAnalysis.deepCopy()
            withUpdates.withUpdating = false
            withUpdates.updateAnalysis(timeout)
        })

        runBenchmarks(
            "incremental analysis without refactoring updates",
            () => {
                val withUpdates = initialAnalysis.deepCopy()
                withUpdates.withUpdating = false
                withUpdates
            },
            (timeout, analysis) => analysis.updateAnalysis(timeout),
            incremental = true)

        val noRefactoringString = resultsNoRefactoring.toCSVString(rows = resultsWithRefactoring.allRows.toList.sortBy(_.toInt))
        val withRefactoringString = resultsWithRefactoring.toCSVString(rows = resultsWithRefactoring.allRows.toList.sortBy(_.toInt))
        val fullRunsString = resultsNonIncremental.toCSVString(rows = resultsWithRefactoring.allRows.toList.sortBy(_.toInt))
        val fullString = "\nIncremental analysis without refactoring updates\n\n" ++ noRefactoringString ++ "\n\nIncremental analysis with refactoring updates\n\n" ++ withRefactoringString + "\n\nFull analysis\n\n" + fullRunsString + "\n\n All Averages\n\n" + resultAverages.toCSVString()
        println(fullString)
        val outFile = new File(writeToFile)
        val bw = new BufferedWriter(new FileWriter(outFile))
        bw.write(fullString)
        bw.close()

        println(resultAverages.prettyString())




    //onBenchmark("test/changeDetectionTest/benchmarks/renamings/browse.scm")
   // onBenchmark("test/changeDetectionTest/benchmarks/Scope Changes/browse.scm")
    val benchmarks = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/renamings")()
    benchmarks.foreach(file =>
        onBenchmark(file)
    )


