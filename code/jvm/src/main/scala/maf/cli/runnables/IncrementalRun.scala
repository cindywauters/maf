package maf.cli.runnables

import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.modular.scheme.modf._
import maf.modular.incremental.IncrementalLogging
import maf.modular.incremental.scheme.SchemeAnalyses._
import maf.util.Reader
import maf.util.benchmarks.Timeout

import scala.concurrent.duration._

object IncrementalRun extends App {

  def modconcAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new IncrementalModConcAnalysisCPLattice(text) {
      override def intraAnalysis(
          cmp: Component
        ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis {
        override def analyzeWithTimeout(timeout: Timeout.T): Unit = {
          println(s"Analyzing $cmp")
          super.analyzeWithTimeout(timeout)
        }
      }
    }
    a.analyzeWithTimeout(timeout())
    print(a.finalResult)
    //a.updateAnalysis(timeout())
  }

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit = {
    def newAnalysis(text: SchemeExp) = new IncrementalSchemeModFAssertionAnalysisCPLattice(text) with IncrementalLogging[SchemeExp] {
      override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
        with IncrementalSchemeModFBigStepIntra
        with IncrementalGlobalStoreIntraAnalysis
        with AssertionModFIntra
        with IncrementalLoggingIntra
    }

    println(s"***** $bench *****")
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = newAnalysis(text)
    a.analyzeWithTimeout(timeout())
    a.printAssertions()
    //val aC = a.deepCopy()
    a.tarjanFlag = false
    a.updateAnalysis(timeout(), true)
    a.printAssertions()
    //aC.updateAnalysis(timeout(), true)
    //aC.printAssertions()
    //val b = newAnalysis(text)
    //b.version = New
    //b.analyzeWithTimeout(timeout())
    //b.printAssertions()
  }

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changes/scheme/assertions/fact.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(30, SECONDS))

  modConcbenchmarks.foreach(modconcAnalysis(_, standardTimeout))
  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
}
