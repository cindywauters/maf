
package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.runnables.ChangeIncremental.res
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.{SchemeChangePatterns, SchemeExp}
import maf.language.scheme.interpreter.SchemeInterpreter
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.ModAnalysis
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.scheme.modf.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.worklist.LIFOWorklistAlgorithm
import maf.util.{Reader, Writer}
import maf.util.Writer.Writer
import maf.util.benchmarks.Timeout
import maf.util.graph.DotGraph
import maf.util.graph.DotGraph.*

import scala.concurrent.duration.*

object TestIncrementalRunDatastructures extends App:

  val w = Writer.open("benchOutput/incremental/errors.txt")

  // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
  // Useful when reducing a program when debugging the analysis.
  def interpretProgram(file: String): Unit =
    val prog = CSchemeParser.parseProgram(Reader.loadFile(file))
    val i = new SchemeInterpreter((_, _) => (), stack = true)
    print("*")
    i.run(prog, Timeout.start(Duration(3, MINUTES)), Old)
    print("*")
    i.run(prog, Timeout.start(Duration(3, MINUTES)), New)
    println("*")

  def modconcAnalysis(
                       bench: String,
                       config: IncrementalConfiguration,
                       timeout: () => Timeout.T
                     ): Unit =
    val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
    val a = new IncrementalModConcAnalysisCPLattice(text, config) with IncrementalLogging[SchemeExp] {
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp)
        with IncrementalSmallStepIntra
        with KCFAIntra
        with IncrementalGlobalStoreIntraAnalysis
        with IncrementalLoggingIntra {
        override def analyzeWithTimeout(timeout: Timeout.T): Unit =
          println(s"Analyzing $cmp")
          super.analyzeWithTimeout(timeout)
      }
    }
    a.analyzeWithTimeout(timeout())
    print(a.finalResult)
  //a.updateAnalysis(timeout())

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =
    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
      new IncrementalSchemeModFAnalysisTypeLattice(text, configuration)
        with IncrementalLogging[SchemeExp]
        with IncrementalDataFlowVisualisation[SchemeExp] {
        override def focus(a: Addr): Boolean = a.toString.toLowerCase().nn.contains("ret")

        override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          //  with AssertionModFIntra
          with IncrementalLoggingIntra
          with IncrementalVisualIntra
      }

    // Analysis from soundness tests.
    def base(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      with SchemeModFNoSensitivity
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain // IncrementalSchemeConstantPropagationDomain
      with IncrementalGlobalStore[SchemeExp]
      // with IncrementalLogging[SchemeExp]
      // with IncrementalDataFlowVisualisation[SchemeExp]
    {
      //override def focus(a: Addr): Boolean =
      //!a.toString.contains("PrmAddr") && (a.toString.contains("ret") || a.toString.contains("x2") || a.toString.contains("__"))
      var configuration: IncrementalConfiguration = allOptimisations
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
      // with IncrementalLoggingIntra
      // with IncrementalVisualIntra
    }

    try {
      println(s"***** $bench *****")
      interpretProgram(bench)
      val text = CSchemeParser.parseProgram(Reader.loadFile(bench))
     // println(text.prettyString())
     /* println(a.mapping)
      println(a.getClass)
      println(a.store)
      println(a.deps)
      println(a.addressDependencies)
      println(a.visited)
      println(a.cachedReadDeps)
      println(a.cachedSpawns)
      println(a.countedSpawns)
      println(a.cachedWrites)
    */

      val a = base(text)
      a.analyzeWithTimeout(timeout())
      println("store before: " + a.store.toString)
      var update = new IncrementalUpdateDatastructures
      update.changeDataStructures(a, text)
      val storeWithUpdate = a.store

      val b = base(text)
      b.analyzeWithTimeout(timeout())
      b.updateAnalysis(timeout())

      val storeWithReanalysis = b.store

      println("Store with updating: " + storeWithUpdate.toString)
      println("Store with reanalysis: " + storeWithReanalysis.toString)

  /*    storeWithReanalysis.foreach((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println(k.toString() + " " + updatedValue.toString +  " " + v.toString)
          case _ =>)
*/

      println("Reanalysis -> Update: " + storeWithReanalysis.forall((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Update -> reanalysis: " + storeWithUpdate.forall((k, v) =>
        storeWithReanalysis.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      /*storeWithReanalysis.foreach((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) => println(updatedValue.==(v))
          case _ => println("missing"))*/

     // a.updateAnalysis(timeout())

    /*  println(a.store)
      println(a.deps)
      println(a.addressDependencies)
      println(a.visited)
      println(a.mapping)
      println(a.cachedReadDeps)
      println(a.cachedSpawns)
      println(a.countedSpawns)
      println(a.cachedWrites)*/

    } catch {
      case e: Exception =>
        e.printStackTrace(System.out)
        Writer.writeln(w, bench)
        Writer.writeln(w, e.getStackTrace().toString)
        Writer.writeln(w, "")
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming.scm") //List("test/changes/scheme/generated/selsort-7.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  //println("Creating graphs")
  //createPNG("logs/flowsA1.dot", true)
  //createPNG("logs/flowsA2.dot", true)
  //createPNG("logs/flowsB.dot", true)
  println("Done")