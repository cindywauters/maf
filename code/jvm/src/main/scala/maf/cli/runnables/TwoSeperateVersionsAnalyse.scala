package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Expression
import maf.language.scheme.SchemeCodeChange
import maf.modular.{AddrDependency, Dependency, ReturnAddr}
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalModAnalysisWithUpdate, IncrementalModAnalysisWithUpdateTwoVersions, IncrementalUpdateDatastructures, UpdateIncrementalSchemeModFBigStepSemantics}
import maf.modular.scheme.SchemeAddr
import maf.util.Writer.close
//import maf.cli.runnables.IncrementalRun.standardTimeout
import maf.core.BasicEnvironment
import maf.language.CScheme.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambdaExp}
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
import java.time.LocalDateTime
import scala.concurrent.duration.*
import maf.modular.incremental.update.SchemeModFSemanticsUpdate
import maf.language.CScheme.CSchemeLexicalAddresser

object TwoSeperateVersionsAnalyse extends App:

  def modfAnalysis(bench: String, timeout: () => Timeout.T): Unit =

    def baseUpdates(oldProgram: SchemeExp, newProgram: SchemeExp) = new ModAnalysis[SchemeExp](oldProgram)
      with StandardSchemeModFComponents
      //with SchemeModFFullArgumentSensitivity
     // with SchemeModFCallSiteSensitivity
      //with SchemeModFFullArgumentCallSiteSensitivity
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
                                ) = new IntraAnalysis(cmp) with UpdateIncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis // {
   /*     override protected def eval(exp: SchemeExp): EvalM[Value] =
          if version == New then
            println("intra")
            println(exp)
            println(allChanges)
          super.eval(exp)
      } */
      }


    try {
      println(s"***** $bench *****")
      val program = CSchemeParserWithSplitter.parseProgram(Reader.loadFile(bench))//CSchemeParser.parseProgram(Reader.loadFile(bench))

      println(program._1.prettyString())
      println(program._2.prettyString())

      println(program._1.subexpressions)


     /* for(i <- 1 to 4) {
        val analysisWithUpdates = baseUpdates(program._1, program._2)
        analysisWithUpdates.analyzeWithTimeout(timeout())
        val beforeUpdateAnalysis = System.nanoTime
        analysisWithUpdates.version = New
        analysisWithUpdates.updateAnalysis(timeout(), true)
        val timeUpdateAnalysis = System.nanoTime - beforeUpdateAnalysis
      }*/

      val analysisWithUpdates = baseUpdates(program._1, program._2)
      analysisWithUpdates.analyzeWithTimeout(timeout())
   //   println(analysisWithUpdates.mapping)
   //   println(analysisWithUpdates.store)
      val beforeUpdateAnalysis = System.nanoTime
      analysisWithUpdates.version = New
     // analysisWithUpdates.analyzeWithTimeout(timeout())
      analysisWithUpdates.updateAnalysis(timeout(), true)
      val timeUpdateAnalysis = System.nanoTime - beforeUpdateAnalysis

      val storeWithUpdate = analysisWithUpdates.store
      val depsWithUpdate = analysisWithUpdates.deps
      val mappingWithUpdate = analysisWithUpdates.mapping
      val visitedWithUpdate = analysisWithUpdates.visited

      println("next")


      val analysisWithoutUpdates = baseUpdates(program._1, program._2)
   //   println(analysisWithoutUpdates.store)
   //   println(analysisWithoutUpdates.mainBody)
   //   println(analysisWithoutUpdates.secondMainBody)

      val beforeNewAnalysis = System.nanoTime
      analysisWithoutUpdates.version = New
      analysisWithoutUpdates.analyzeWithTimeout(timeout())
      val timeNewAnalysis = System.nanoTime - beforeNewAnalysis

      val storeWithoutUpdate = analysisWithoutUpdates.store
      val depsWithoutUpdate = analysisWithoutUpdates.deps
      val mappingWithoutUpdate = analysisWithoutUpdates.mapping
      val visitedWithoutUpdate = analysisWithoutUpdates.visited

      println("updating done")
      println("Time updating:                    " + timeUpdateAnalysis)
      println("Time analysis new:                " + timeNewAnalysis)

     // println("Store with update: " + storeWithUpdate.toString)
     // println("Store new only   : " + storeWithoutUpdate.toString)

      println("store reanalysis -> Update (subsumption): " + storeWithoutUpdate.forall((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) =>
            if !analysisWithUpdates.lattice.subsumes(updatedValue, v) then
              println("store r -> u " + k.toString() + " " + v.toString + " " + updatedValue.toString)
            analysisWithUpdates.lattice.subsumes(updatedValue, v)
          case _ =>
            println("old: " + v.toString + " " + k.toString())
            false).toString)
      println("all missing: ")
      storeWithoutUpdate.foreach((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) =>
            if !analysisWithUpdates.lattice.subsumes(updatedValue, v) then
              println("store r -> u " + k.toString() + " " + v.toString + " " + updatedValue.toString)
              println(storeWithUpdate.zipWithIndex.find((k1, v1) => k == k1._1))
              println(storeWithoutUpdate.zipWithIndex.find((k1, v1) => k == k1._1))
            analysisWithUpdates.lattice.subsumes(updatedValue, v)
          case _ =>
            println("old: " + v.toString + " " + k.toString())
            false)

    //  println("Dependencies with update: " + depsWithUpdate.toString)
    //  println("Dependencies new only   : " + depsWithoutUpdate.toString)

      println("Dependencies reanalysis -> Update (subsumption): " + depsWithoutUpdate.forall((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) =>
            if !v.forall(elv => updatedValue.contains(elv)) then
              println(" deps r -> u " + k.toString() + " " + v.toString + " " + updatedValue.toString)
              v.foreach(e => e match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println(lam.toString + " " + env.toString + " " + oldCtx.toString))
              updatedValue.foreach(e => e match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println(lam.toString + " " + env.toString + " " + oldCtx.toString)  )
            v.forall(elv => updatedValue.contains(elv))
          case _ =>
            println("doesnt exist: ")
            println(k)
            false).toString)

    //  println("Mapping with update : " + mappingWithUpdate.toString)
    //  println("Mapping new only    : " + mappingWithoutUpdate.toString)

      println(mappingWithoutUpdate.size)
      println("Mapping reanalysis -> Update (subsumption): " + mappingWithoutUpdate.forall((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) =>
            if !v.forall(elv => updatedValue.contains(elv)) then
              println(k)
              println(v)
              println(updatedValue)
              v.foreach(e => e match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println("without")
                  println(lam.idn.toString + " " + lam.toString + " " + env.content.toString + " " + oldCtx.toString)
                case _ =>)
              updatedValue.foreach(e => e match
                    case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                      println("with")
                      println(lam.idn.toString + " " + lam.toString + " " + env.content.toString + " " + oldCtx.toString)
                    case _ =>)
            v.forall(elv => updatedValue.contains(elv))
          case _ =>
            println("no matching key")
            println(k)
            println(v)
            false).toString)

      println("Visited with update : " + visitedWithUpdate.toString)
      println("Visited new only    : " + visitedWithoutUpdate.toString)
      println("Visited reanalysis -> Update (subsumption): " + visitedWithoutUpdate.forall(e =>
          /* println(e)
           e match
             case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
              println(lam.idn.toString + " " + lam.toString + " " + env.content.toString + " " + oldCtx.toString)
             case _ =>*/
           visitedWithUpdate.contains(e)).toString)

   /* visitedWithUpdate.foreach(e => e match
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
        println(lam.idn.toString + " " + lam.toString + " " + env.content.toString + " " + oldCtx.toString)
        println(lam.fv)
      case _ =>
    )

    println()

    visitedWithoutUpdate.foreach(e => e match
        case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
          println(lam.idn.toString + " " + lam.toString + " " + env.content.toString + " " + oldCtx.toString)
        case _ =>
      )*/


      println(storeWithUpdate.size + " (" + storeWithoutUpdate.size + ")")
      println(depsWithUpdate.size + "  (" + depsWithoutUpdate.size + ")")
      println(mappingWithUpdate.size + " (" + mappingWithoutUpdate.size + ")")
      println(visitedWithUpdate.size + "  (" + visitedWithoutUpdate.size + ")")



    } catch {
      case e: Exception =>
        println(e)
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  // val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm", "test/changeDetectionTest/onlyConsistentRenaming/Vectors.scm", "test/changeDetectionTest/onlyConsistentRenaming/Lists.scm")
 // val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/R5RS/various/NoSensitivity/SICP-compiler.scm")
  //val modFbenchmarks: List[String] = List("test/changes/scheme/browse.scm")
  val modFbenchmarks: List[String] = List("test/changeDetectionTest/scopeChangesManual/machine-simulator.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/scopeChangesManual/gambit_browse.scm")
 // val modFbenchmarks: List[String] = List("test/changeDetectionTest/scopeChangesManual/gambit_nboyer.scm")
 // val modFbenchmarks: List[String] = List("test/changeDetectionTest/testsWithUpdate/findScopeChanges.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/mixOfChanges/R5RS/gambit/NoSensitivity/earley.scm")

  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  println("Done")