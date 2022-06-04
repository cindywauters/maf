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
import maf.modular.worklist.{FIFOWorklistAlgorithm, LIFOWorklistAlgorithm}
import maf.util.Reader
import maf.util.benchmarks.{Statistics, Table, Timeout, Timer}

import java.io.{BufferedWriter, File, FileWriter}
import scala.concurrent.duration.{Duration, MINUTES}

object UpdatingPerformance extends App:
    def AnalysisType(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
        with StandardSchemeModFComponents
        with SchemeModFNoSensitivity
        with SchemeModFSemanticsUpdate
        with FIFOWorklistAlgorithm[SchemeExp]
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

    var warmup = 10//10
    var rounds = 25//25

    def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))

    var resultsNoRefactoring: Table[Int] = Table.empty.withDefaultValue(0)
    var resultsWithRefactoring: Table[Int] = Table.empty.withDefaultValue(0)
    var resultsNonIncremental: Table[Int] = Table.empty.withDefaultValue(0)
    var resultAverages: Table[Int] = Table.empty.withDefaultValue(0)
    var resultStdDev: Table[Double] = Table.empty.withDefaultValue(0)

    def addToIncrementalInitial(table: Table[Int], a: IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp], index: String, totalTime: Double): Table[Int] =
        var outputTable = table.add(index, "total time", totalTime.toInt)
        outputTable = outputTable.add(index, "change detection", (a.timeFindingChanges / 1000000).toInt)
        outputTable = outputTable.add(index, "updating datastructures", (a.timeUpdatingStructures / 1000000).toInt)
        outputTable = outputTable.add(index, "incremental analysis" , (a.timeIncrementalReanalysis / 1000000).toInt)
        outputTable

    def addToAverages(withRefactoring: Boolean, allTimes: List[Double],  allFinding: List[Double], allUpdating: List[Double], allAnalysis: List[Double]): Unit =
        val row = if withRefactoring then "With updating" else "No updating"

        def addToOne(stats: Statistics.Stats, typeTime: String): Unit =
            resultAverages = resultAverages.add(row, "avg " + typeTime, stats.mean.toInt)
            resultStdDev = resultStdDev.add(row, "std dev " + typeTime, stats.stddev)

        addToOne(Statistics.all(allTimes), "full")
        addToOne(Statistics.all(allFinding), "finding")
        addToOne(Statistics.all(allUpdating), "updating")
        addToOne(Statistics.all(allAnalysis), "incremental analysis")


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
        var timesFinding: List[Double] = List()
        var timesUpdating: List[Double] = List()
        var timesIncremental: List[Double] = List()
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
                        timesFinding     = (analysis.timeFindingChanges / 1000000).toDouble :: timesFinding
                        timesUpdating    = (analysis.timeUpdatingStructures / 1000000).toDouble :: timesUpdating
                        timesIncremental = (analysis.timeIncrementalReanalysis / 1000000).toDouble :: timesIncremental
                case None =>
                    println(" timed out.")
                    return None
            println()
        if incremental then
            addToAverages(withRefactorings,times, timesFinding, timesUpdating, timesIncremental)
        else
            val stats = Statistics.all(times)
            if oldVersion then
                resultAverages = resultAverages.add("Intial analysis", "avg full", stats.mean.toInt)
                resultStdDev = resultStdDev.add("Intial analysis", "std dev full", stats.stddev)
            else
                resultAverages = resultAverages.add("New analysis", "avg full", stats.mean.toInt)
                resultStdDev = resultStdDev.add("New analysis", "std dev full", stats.stddev)
        Some(times)

    def onBenchmark(file: String): Unit =
        val (oldProgram, newProgram) = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(file))
        val fullfilename = file.split("\\\\|\\.|\\/")
      //  println(file)
      //  println(fullfilename)
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
            (timeout, analysis) => analysis.analyzeWithTimeout(timeout),
            oldVersion = true)

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
            (timeout, analysis) => analysis.analyzeWithTimeout(timeout))


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
        val fullString = "\nIncremental analysis without refactoring updates\n\n" ++ noRefactoringString ++ "\n\nIncremental analysis with refactoring updates\n\n" ++ withRefactoringString + "\n\nFull analysis\n\n" + fullRunsString + "\n\n All Standard devs\n\n" + resultStdDev.toCSVString() + "\n\n All Averages\n\n" + resultAverages.toCSVString()
        println(fullString)
        val outFile = new File(writeToFile)
        val bw = new BufferedWriter(new FileWriter(outFile))
        bw.write(fullString)
        bw.close()

        val initialFull = resultAverages.get("Intial analysis", "avg full").getOrElse(0).toString
        val newFull = resultAverages.get("New analysis", "avg full").getOrElse(0).toString
        val noUp = resultAverages.get("No updating", "avg full").getOrElse(0).toString
        val wiUp = resultAverages.get("With updating", "avg full").getOrElse(0).toString

        if fullfilename != null then
            val dir = fullfilename(3)
            val filename = fullfilename(4)
            val outFileAvg = new File("benchOutput/UpdatingPerformance/avgs_new.csv")
            val bwavg = new BufferedWriter(new FileWriter(outFileAvg, true))
            bwavg.write("\n" + dir.toString + " " + filename.toString + "," + initialFull + "," + newFull + "," + noUp + "," + wiUp)
            bwavg.close()

        println(resultAverages.prettyString())




    //onBenchmark("test/changeDetectionTest/benchmarks/renamings/browse.scm")
   // onBenchmark("test/changeDetectionTest/benchmarks/Scope Changes/browse.scm")
    //val benchmarks = SchemeBenchmarkPrograms.fromFolder("test/changeDetectionTest/benchmarks/renamings")()
    val benchmarks: List[String] = List(/*"test/changeDetectionTest/benchmarks/scope changes/nbody-processed.scm",
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
         "test/changeDetectionTest/benchmarks/renamings/multiple-dwelling.scm",*/
         "test/changeDetectionTest/benchmarks/scope changes/machine-simulator.scm",
         "test/changeDetectionTest/benchmarks/ifs/machine-simulator.scm",
         "test/changeDetectionTest/benchmarks/renamings/machine-simulator.scm"
   )
    benchmarks.foreach(file =>
        onBenchmark(file)
    )


