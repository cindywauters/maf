
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


  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =
    def newAnalysis(text: SchemeExp, configuration: IncrementalConfiguration) =
      new IncrementalSchemeModFAnalysisTypeLattice(text, configuration)
        with IncrementalLogging[SchemeExp]
        with IncrementalDataFlowVisualisation[SchemeExp] {
        override def focus(a: Addr): Boolean = a.toString.toLowerCase().nn.contains("ret")

        override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp)
          with IncrementalSchemeModFBigStepIntra
          with IncrementalGlobalStoreIntraAnalysis
          with IncrementalLoggingIntra
          with IncrementalVisualIntra
      }

    // Analysis from soundness tests.
    def base(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      with SchemeModFFullArgumentSensitivity
    //  with SchemeModFNoSensitivity
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain
      with IncrementalGlobalStore[SchemeExp]
    {
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
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

      val storeBefore = a.store
      val depsBefore = a.deps
      val mappingBefore = a.mapping
      val visitedBefore = a.visited
      //TODO: implicitFlows, cachedWrites, cachedReadDeps, dataFlowR, cachedSpawns, provenance


      var update = new IncrementalUpdateDatastructures
      update.changeDataStructures(a, text)
      val storeWithUpdate = a.store
      val depsWithUpdate = a.deps
      val mappingWithUpdate = a.mapping
      val visitedWithUpdate = a.visited

      val b = base(text)
      b.version = New
      b.analyzeWithTimeout(timeout())

      val storeWithReanalysis = b.store
      val depsWithReanalysis = b.deps
      val mappingWithReanalysis = b.mapping
      val visitedWithReanalysis = b.visited


      println("store before: " + storeBefore.toString)
      println("Store with updating: " + storeWithUpdate.toString)
      println("Store with reanalysis: " + storeWithReanalysis.toString)


      println("store reanalysis -> Update: " + storeWithReanalysis.forall((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("store update -> reanalysis: " + storeWithUpdate.forall((k, v) =>
        storeWithReanalysis.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)


      storeWithReanalysis.foreach((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + " " + k.idn.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>println("missing in update: " + k.toString() + " " + k.idn.toString() + "\n reanalysis value: " + v.toString))


      storeWithUpdate.foreach((k, v) =>
        storeWithReanalysis.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + " " + k.idn.toString() + "\n update value: " + v.toString)
      )

      println()

      println("Dependencies before: " + depsBefore.toString)
      println("Dependencies with updating: " + depsWithUpdate.toString)
      println("Dependencies with reanalysis: " + depsWithReanalysis.toString)


      println("Dependencies reanalysis -> Update: " + depsWithReanalysis.forall((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Dependencies update -> reanalysis: " + depsWithUpdate.forall((k, v) =>
        depsWithReanalysis.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      depsWithReanalysis.foreach((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>println("missing in update: " + k.toString()  + "\n reanalysis value: " + v.toString))


      depsWithUpdate.foreach((k, v) =>
        depsWithReanalysis.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + "\n update value: " + v.toString)
      )

     println()

      println()

      println("Mapping before: " + mappingBefore.toString)
      println("Mapping with updating: " + mappingWithUpdate.toString)
      println("Mapping with reanalysis: " + mappingWithReanalysis.toString)


      println("Mapping reanalysis -> Update: " + mappingWithReanalysis.forall((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Mapping update -> reanalysis: " + mappingWithUpdate.forall((k, v) =>
        mappingWithReanalysis.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      mappingWithReanalysis.foreach((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>println("missing in update: " + k.toString()  + "\n reanalysis value: " + v.toString))


      mappingWithUpdate.foreach((k, v) =>
        mappingWithReanalysis.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + "\n update value: " + v.toString)
      )

      println()

      println()

      println("Visited before: " + visitedBefore.toString)
      println("Visited with updating: " + visitedWithUpdate.toString)
      println("Visited with reanalysis: " + visitedWithReanalysis.toString)


      println("Visited reanalysis -> Update: " + visitedWithReanalysis.forall(e => visitedWithUpdate.contains(e)).toString)

      println("Visited update -> reanalysis: " + visitedWithUpdate.forall(e => visitedWithReanalysis.contains(e)).toString)

      visitedWithReanalysis.foreach(e =>
        if !visitedWithUpdate.contains(e) then
          println("missing in update: " + e.toString()))


      visitedWithUpdate.foreach(e =>
        if !visitedWithReanalysis.contains(e) then
          println("missing in reanalysis: " + e.toString()))

      println()


    } catch {
      case e: Exception =>
        e.printStackTrace(System.out)
        Writer.writeln(w, bench)
        Writer.writeln(w, e.getStackTrace().toString)
        Writer.writeln(w, "")
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm", "test/changeDetectionTest/ConRenamingVectors.scm", "test/changeDetectionTest/ConRenamingLists.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingVectors.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLists.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenamingProblems.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  println("Done")