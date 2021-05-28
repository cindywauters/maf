package maf.cli.experiments.parallel

import maf.core._
import maf.language.scheme._
import maf.language.CScheme._
import maf.util._
import maf.util.benchmarks._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._

trait Metric {
  def name: String
  def forProgram(program: SchemeExp): Metric.SequenceBasedMetric
}

object Metric {
  case class SequenceBasedMetric(vs: List[Double]) {
    def mean = Statistics.mean(vs)
    def median = Statistics.median(vs)
    def stddev = Statistics.stddev(vs)
    def max = vs.max
    def add(v: Double): SequenceBasedMetric = SequenceBasedMetric(v :: vs)
    override def toString = s"$mean [$median±$stddev] <= $max"
  }

  object ExpressionDepth extends Metric {
    type M = SequenceBasedMetric
    def name = "exp-depth"
    def computeDepths(exp: Expression, depths: Map[Identity, Int] = Map.empty): Map[Identity, Int] =
      exp.subexpressions
        .foldLeft(Map.empty[Identity, Int].withDefaultValue(0))((depths, exp) => computeDepths(exp, depths))
        .map({ case (k, v) => (k, v + 1) }) ++ depths + (exp.idn -> 0)
    def forProgram(program: SchemeExp): M =
      SequenceBasedMetric(computeDepths(program).values.toList.map(_.toDouble))
  }

  class CallDepth(val kCFA: Int) extends Metric {
    def name = s"call-depth"
    type M = SequenceBasedMetric

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
        with SchemeModFSemantics
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with CallDepthFirstWorklistAlgorithm[SchemeExp]
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.depth.values.toList.map(_.toDouble))
    }
  }

  class LeastVisited(kCFA: Int) extends Metric {
    def name = s"least-visited"
    type M = SequenceBasedMetric

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
        with SchemeModFSemantics
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with LeastVisitedFirstWorklistAlgorithm[SchemeExp]
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.count.values.toList.map(_.toDouble))
    }
  }
  class MostVisited(kCFA: Int) extends Metric {
    def name = s"most-visited"
    type M = SequenceBasedMetric

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
        with SchemeModFSemantics
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with MostVisitedFirstWorklistAlgorithm[SchemeExp]
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.count.values.toList.map(_.toDouble))
    }
  }

  class NumberOfDependencies(kCFA: Int) extends Metric {
    type M = SequenceBasedMetric
    def name = s"deps"

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
        with SchemeModFSemantics
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with MostDependenciesFirstWorklistAlgorithm[SchemeExp]
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.depCount.values.toList.map(_.toDouble))
    }
  }

  class EnvironmentSize(kCFA: Int) extends Metric {
    type M = SequenceBasedMetric
    def name = s"env-size"

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
        with SchemeModFSemantics
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with BiggerEnvironmentFirstWorklistAlgorithm.ModF
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.visited.map(analysis.environmentSize).toList.map(_.toDouble))
    }
  }

  class RatioOfDependencies(kCFA: Int) extends Metric {
    type M = SequenceBasedMetric
    def name = s"dep-ratio"

    def forProgram(program: SchemeExp): M = {
      val analysis = new ModAnalysis(program)
        with SchemeModFSemantics
        with StandardSchemeModFComponents
        with BigStepModFSemantics
        with TriggerRegisterRatioWorklistAlgorithm[SchemeExp]
        with SchemeModFKCallSiteSensitivity
        with SchemeConstantPropagationDomain {
        val k = kCFA
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra
      }
      analysis.analyze()
      SequenceBasedMetric(analysis.ratios.values.toList)
    }
  }
}

trait Metrics {
  def benchmarks: Iterable[String]

  def metrics: List[Metric]

  var results = Table.empty[Metric.SequenceBasedMetric].withDefaultValue(Metric.SequenceBasedMetric(List()))

  def metricForFile(file: String, metric: Metric): Metric.SequenceBasedMetric = {
    val program = CSchemeParser.parse(Reader.loadFile(file))
    metric.forProgram(program)
  }

  def paperName: Map[String, String] = List(
    ("test/R5RS/WeiChenRompf2019/meta-circ.scm", "meta-circ"),
    ("test/R5RS/WeiChenRompf2019/earley.sch", "earley"),
    ("test/R5RS/WeiChenRompf2019/toplas98/graphs.scm", "graphs"),
    ("test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm", "dynamic"),
    ("test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm", "nbody"),
    ("test/R5RS/WeiChenRompf2019/toplas98/boyer.scm", "boyer"),
    ("test/R5RS/gambit/peval.scm", "peval"),
    ("test/R5RS/gambit/scheme.scm", "scheme"),
    ("test/R5RS/gambit/sboyer.scm", "sboyer"),
    ("test/R5RS/gambit/nboyer.scm", "nboyer"),
    ("test/R5RS/gambit/matrix.scm", "matrix"),
    ("test/R5RS/gambit/browse.scm", "browse"),
    ("test/R5RS/scp1-compressed/all.scm", "scp"),
    ("test/R5RS/ad/all.scm", "ad"),
    ("test/R5RS/various/SICP-compiler.scm", "SICP"),
    ("test/R5RS/icp/icp_1c_ambeval.scm", "ambeval"),
    ("test/R5RS/icp/icp_1c_multiple-dwelling.scm", "multiple-dwelling"),
    ("test/R5RS/icp/icp_1c_ontleed.scm", "decompose"),
    ("test/R5RS/icp/icp_1c_prime-sum-pair.scm", "prime-sum-pair"),
    ("test/R5RS/icp/icp_7_eceval.scm", "eceval"),
    ("test/R5RS/icp/icp_8_compiler.scm", "compiler"),
    ("test/R5RS/icp/icp_5_regsim.scm", "regsim"),
    ("test/R5RS/icp/icp_3_leval.scm", "leval"),
    ("test/R5RS/icp/icp_2_aeval.scm", "aeval")
  ).toMap

  def metricsForFile(file: String): Unit =
    metrics.foreach { metric =>
      try {
        println(s"***** Computing metric ${metric.name} on $file *****")
        val result = metricForFile(file, metric)
        println(result)
        results = results.add(paperName(file), metric.name, result)
      } catch {
        case e: Exception =>
          println(s"Encountered an exception: ${e.getMessage}")
        case e: VirtualMachineError =>
          System.gc()
          println(s"VM Error: ${e.getMessage}")
      }
    }

  def printResults() =
    println(results.prettyString())
  def exportCSV(
      path: String,
      format: Metric.SequenceBasedMetric => String,
      timestamped: Boolean = true
    ) = {
    val hdl = if (timestamped) Writer.openTimeStamped(path) else Writer.open(path)
    val csv = results.toCSVString(format = format)
    Writer.write(hdl, csv)
    Writer.close(hdl)
  }

  def run(path: String = "benchOutput/metrics/output.csv") = {
    benchmarks.foreach(metricsForFile)
    printResults()
    exportCSV(path, format = _.toString())
  }
}

trait ParallelMetrics extends Metrics {
  def k: Int
  def metrics = List(
    Metric.ExpressionDepth,
    new Metric.CallDepth(k),
    new Metric.LeastVisited(k),
    new Metric.MostVisited(k),
    new Metric.NumberOfDependencies(k),
    new Metric.RatioOfDependencies(k),
    new Metric.EnvironmentSize(k)
  )
  def formatMean(m: Metric.SequenceBasedMetric): String = m.mean.toString()
  def formatStddev(m: Metric.SequenceBasedMetric): String = m.stddev.toString()
  def formatMax(m: Metric.SequenceBasedMetric): String = m.max.toString()
}

object ParallelMetrics0CFA extends ParallelMetrics {
  def k = 0
  def benchmarks = ParallelModFBenchmarks.all
  def main(args: Array[String]): Unit = {
    run()
    exportCSV("data/modf-context-insensitive-metrics-mean.csv", formatMean _, timestamped = false)
    exportCSV("data/modf-context-insensitive-metrics-stddev.csv", formatStddev _, timestamped = false)
    exportCSV("data/modf-context-insensitive-metrics-max.csv", formatMax _, timestamped = false)
  }
}

object ParallelMetrics1CFA extends ParallelMetrics {
  def k = 1
  def benchmarks = ParallelModFBenchmarks.all
  def main(args: Array[String]): Unit = {
    run()
    exportCSV("data/modf-context-insensitive-metrics-1CFA-mean.csv", formatMean _, timestamped = false)
    exportCSV("data/modf-context-insensitive-metrics-1CFA-stddev.csv", formatStddev _, timestamped = false)
    exportCSV("data/modf-context-insensitive-metrics-1CFA-max.csv", formatMax _, timestamped = false)
  }
}

object ParallelMetrics2CFA extends ParallelMetrics {
  def k = 2
  def benchmarks = ParallelModFBenchmarks.for2CFA
  def main(args: Array[String]): Unit = {
    run()
    exportCSV("data/modf-context-sensitive-metrics-2CFA-mean.csv", formatMean _, timestamped = false)
    exportCSV("data/modf-context-sensitive-metrics-2CFA-stddev.csv", formatStddev _, timestamped = false)
    exportCSV("data/modf-context-sensitive-metrics-2CFA-max.csv", formatMax _, timestamped = false)
  }

}
