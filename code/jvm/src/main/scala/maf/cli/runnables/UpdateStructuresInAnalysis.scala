
package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Expression
import maf.modular.{AddrDependency, Dependency}
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain
import maf.modular.incremental.update.{IncrementalGlobalStoreWithUpdate, IncrementalUpdateDatastructures}
import maf.modular.scheme.SchemeAddr
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

object UpdateStructuresInAnalysis extends App:

  //val w = Writer.open("benchOutput/incremental/errors.txt")

  // Runs the program with a concrete interpreter, just to check whether it makes sense (i.e., if the concrete interpreter does not error).
  // Useful when reducing a program when debugging the analysis.
  def interpretProgram(file: String): Unit =
    val prog = CSchemeParser.parseProgram(Reader.loadFile(file))
    val i = new SchemeInterpreter((_, _) => ())
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
    def baseNoUpdates(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      //with SchemeModFFullArgumentSensitivity
      //with SchemeModFCallSiteSensitivity
      //with SchemeModFFullArgumentCallSiteSensitivity
      with SchemeModFNoSensitivity
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

    def baseUpdates(program: SchemeExp) = new ModAnalysis[SchemeExp](program)
      with StandardSchemeModFComponents
      // with SchemeModFFullArgumentSensitivity
     // with SchemeModFCallSiteSensitivity
    //  with SchemeModFFullArgumentCallSiteSensitivity
      with SchemeModFNoSensitivity
      with SchemeModFSemanticsM
      with LIFOWorklistAlgorithm[SchemeExp]
      with IncrementalSchemeModFBigStepSemantics
      with IncrementalSchemeTypeDomain
      with IncrementalGlobalStore[SchemeExp]
     // with IncrementalGlobalStoreWithUpdate[SchemeExp]
    {
      var configuration: IncrementalConfiguration = noOptimisations
      override def intraAnalysis(
                                  cmp: Component
                                ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis
    }

    try {
      println(s"***** $bench *****")
    //  interpretProgram(bench)
      val program = CSchemeParser.parseProgram(Reader.loadFile(bench))

      println(program.prettyString())

      // analysisWithoutUpdates.analyzeWithTimeout(timeout())
      // analysisWithoutUpdates.analyzeWithTimeout(timeout())

      for (i <- 1 to 10) {
        val analysisWithoutUpdates = baseNoUpdates(program)
        val beforeAnalysis = System.nanoTime
        analysisWithoutUpdates.version = New
        analysisWithoutUpdates.analyzeWithTimeout(timeout())
        val timeAnalysis = System.nanoTime - beforeAnalysis
      }


      val analysisWithUpdates = baseUpdates(program)
      analysisWithUpdates.analyzeWithTimeout(timeout())
      println(analysisWithUpdates.mapping)
      val beforeUpdateAnalysis = System.nanoTime
      analysisWithUpdates.updateAnalysis(timeout())
      val timeUpdateAnalysis = System.nanoTime - beforeUpdateAnalysis

      val storeWithUpdate = analysisWithUpdates.store
      val depsWithUpdate = analysisWithUpdates.deps
      val mappingWithUpdate = analysisWithUpdates.mapping
      val visitedWithUpdate = analysisWithUpdates.visited

      println("updating done")
      val analysisWithoutUpdates = baseNoUpdates(program)
      val beforeAnalysis = System.nanoTime
      analysisWithoutUpdates.version = New
      analysisWithoutUpdates.analyzeWithTimeout(timeout())
      val timeAnalysis = System.nanoTime - beforeAnalysis

      val storeWithoutUpdate = analysisWithoutUpdates.store
      val depsWithoutUpdate = analysisWithoutUpdates.deps
      val mappingWithoutUpdate = analysisWithoutUpdates.mapping
      val visitedWithoutUpdate = analysisWithoutUpdates.visited

     /* visitedWithoutUpdate.foreach(e =>
        println(e)
          e match
          case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
            println(env)
          case _ =>)*/

      println("first analysis done")

      println("Time incremental analysis:    " + timeAnalysis)
      println("Time updating:                " + timeUpdateAnalysis)


      // println("Store with updating: " + storeWithUpdate.toString)
      //   println("Store with regular reanalysis: " + storeWithoutUpdate.toString)


      println("store reanalysis -> Update: " + storeWithoutUpdate.forall((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("store update -> reanalysis: " + storeWithUpdate.forall((k, v) =>
        storeWithoutUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

     /* storeWithoutUpdate.foreach((k, v) =>
        storeWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("difference key: " + k.toString() + " " + k.idn.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
          case _ =>
            println("missing in update: " + k.toString() + " " + k.idn.toString() + "\n reanalysis value: " + v.toString)
              k match
              case key: maf.modular.ReturnAddr[_] =>
                key.cmp match
                  case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                    println(lam.toString + " " + env.toString + " " + oldCtx.toString)
              case _ =>
      )


      storeWithUpdate.foreach((k, v) =>
        storeWithoutUpdate.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + " " + k.idn.toString() + "\n update value: " + v.toString)
            k match
            case key: maf.modular.ReturnAddr[_] =>
              key.cmp match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println(lam.toString + " " + env.toString + " " + oldCtx.toString)
            case _ =>
      )
*/
  /*    println()

      //println("Dependencies with updating: " + depsWithUpdate.toString)
      //  println("Dependencies with regular reanalysis: " + depsWithoutUpdate.toString)


      println("Dependencies reanalysis -> Update: " + depsWithoutUpdate.forall((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Dependencies update -> reanalysis: " + depsWithUpdate.forall((k, v) =>
        depsWithoutUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("subsumbtion? " + checkSubsumptionSetOfDependencies())

      depsWithoutUpdate.foreach((k, v) =>
        depsWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
              v.head match
              case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                println(lam.toString + " " + env.toString + " " + oldCtx.toString)
              case _ =>
              updatedValue.head match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println(lam.toString + " " + env.toString + " " + oldCtx.toString)
                case _ =>
          case _ =>println("missing in update: " + k.toString()  + "\n reanalysis value: " + v.toString))


      depsWithUpdate.foreach((k, v) =>
        depsWithoutUpdate.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + "\n update value: " + v.toString)
      )


      println()

//      println("Mapping with updating: " + mappingWithUpdate.toString)
//      println("Mapping with regular reanalysis: " + mappingWithoutUpdate.toString)


      println("Mapping reanalysis -> Update: " + mappingWithoutUpdate.forall((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      println("Mapping update -> reanalysis: " + mappingWithUpdate.forall((k, v) =>
        mappingWithoutUpdate.get(k) match
          case Some(updatedValue) => updatedValue.==(v)
          case _ => false).toString)

      def checkSubsumptionContexts(ac: Any, uc: Any): Boolean =
        analysisWithUpdates match
          case a: IncrementalGlobalStore[SchemeExp] =>
            (ac, uc) match
              case (ac: maf.modular.scheme.modf.ArgCallSiteContext, uc: maf.modular.scheme.modf.ArgCallSiteContext) =>
                ac.args.forall(v =>
                  uc.args.exists(w =>
                    a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
              case (ac: maf.modular.scheme.modf.ArgContext, uc: maf.modular.scheme.modf.ArgContext)  =>
                ac.values.forall(v =>
                  uc.values.exists(w => a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
              case (Some(ac: maf.modular.scheme.modf.ArgCallSiteContext), Some(uc: maf.modular.scheme.modf.ArgCallSiteContext)) =>
                ac.args.forall(v =>
                  uc.args.exists(w =>
                    a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
              case (Some(ac: maf.modular.scheme.modf.ArgContext), Some(uc: maf.modular.scheme.modf.ArgContext))  =>
                ac.values.forall(v =>
                  uc.values.exists(w => a.lattice.subsumes(w.asInstanceOf[a.Value], v.asInstanceOf[a.Value])))
              case (ac: _, uc: _) =>
                ac.equals(uc)


      def checkSubsumptionOneComponent(ac: analysisWithoutUpdates.Component, uc: analysisWithUpdates.Component): Boolean =
        analysisWithUpdates match
          case a: IncrementalGlobalStore[SchemeExp] =>
            (ac, uc) match
              case (SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _), SchemeModFComponent.Call((wlam: SchemeLambdaExp, wenv: BasicEnvironment[_]), woldCtx: _)) =>
                if lam.equals(wlam) && wenv.equals(wenv) then
                  checkSubsumptionContexts(oldCtx, woldCtx)
                else false
              case (ac: _, uc: _) => ac.equals(uc)
          case _ => false

      def checkSubsumptionSetOfComponents(ac: Set[analysisWithoutUpdates.Component], uc: Set[analysisWithUpdates.Component]): Boolean =
        analysisWithUpdates match
          case a: IncrementalGlobalStore[SchemeExp] =>
            ac.forall(av =>
              uc.exists(uv =>
                checkSubsumptionOneComponent(av, uv)))
          case _ => false

      def checkSubsumptionSetOfDependencies(): Boolean =
        analysisWithUpdates match
          case a: IncrementalGlobalStore[SchemeExp] =>
            val ac = analysisWithoutUpdates.deps
            val uc = analysisWithUpdates.deps
            ac.forall(av =>
              uc.exists(uv =>
                (av._1, uv._1) match
                  case (k1: AddrDependency, k2: AddrDependency) =>
                    (k1.addr, k2.addr) match
                      case (addr1: maf.modular.scheme.VarAddr[_], addr2: maf.modular.scheme.VarAddr[_]) =>
                        if addr1.equals(addr2) then
                          true
                        else
                          addr1.idn.equals(addr2.idn) && addr1.id.equals(addr2.id) && checkSubsumptionContexts(addr1.ctx, addr2.ctx) && checkSubsumptionSetOfComponents(av._2, uv._2)
                      case (addr1: maf.modular.ReturnAddr[_], addr2: maf.modular.ReturnAddr[_]) =>
                        if addr1.equals(addr2) then
                          true
                        else
                          addr1.idn.equals(addr2.idn) && checkSubsumptionOneComponent(addr1.cmp.asInstanceOf[analysisWithoutUpdates.Component], addr2.cmp.asInstanceOf[analysisWithUpdates.Component]) && checkSubsumptionSetOfComponents(av._2, uv._2)
                      //println("stuck later")
                      //true
                      case (addr1: maf.modular.scheme.PtrAddr[_], addr2: maf.modular.scheme.PtrAddr[_]) =>
                        if addr1.equals(addr2) then
                          true
                        else addr1.idn.equals(addr2.idn) && checkSubsumptionContexts(addr1.ctx, addr2.ctx) && checkSubsumptionSetOfComponents(av._2, uv._2)
                      case (addr1: _, addr2: _) =>
                        addr1.equals(addr2)
                  case _ => false))

      def checkSubsumptionSetOfStore(): Boolean =
        analysisWithUpdates match
          case a: IncrementalGlobalStore[SchemeExp] =>
            val ac = analysisWithoutUpdates.store
            val uc = analysisWithUpdates.store
            ac.forall(av =>
              uc.exists(uv =>
                (av._1, uv._1) match
                  case (addr1: maf.modular.scheme.VarAddr[_], addr2: maf.modular.scheme.VarAddr[_]) =>
                    if addr1.equals(addr2) then
                      true
                    else
                      addr1.idn.equals(addr2.idn) && addr1.id.equals(addr2.id) && checkSubsumptionContexts(addr1.ctx, addr2.ctx) && analysisWithoutUpdates.lattice.subsumes(uv._2, av._2)
                  case (addr1: maf.modular.ReturnAddr[_], addr2: maf.modular.ReturnAddr[_]) =>
                    if addr1.equals(addr2) then
                      true
                    else
                      addr1.idn.equals(addr2.idn) && checkSubsumptionOneComponent(addr1.cmp.asInstanceOf[analysisWithoutUpdates.Component], addr2.cmp.asInstanceOf[analysisWithUpdates.Component]) && analysisWithoutUpdates.lattice.subsumes(uv._2, av._2)
                  case (addr1: maf.modular.scheme.PtrAddr[_], addr2: maf.modular.scheme.PtrAddr[_]) =>
                    if addr1.equals(addr2) then
                      true
                    else addr1.idn.equals(addr2.idn) && checkSubsumptionContexts(addr1.ctx, addr2.ctx) && analysisWithoutUpdates.lattice.subsumes(uv._2, av._2)
                  case (addr1: _, addr2: _) =>
                    addr1.equals(addr2) &&  analysisWithoutUpdates.lattice.subsumes(uv._2, av._2)
                  case _ => false))

      mappingWithoutUpdate.foreach((k, v) =>
        mappingWithUpdate.get(k) match
          case Some(updatedValue) =>
            if updatedValue.!=(v) then
              println("key reanalysis: " + k.toString() + " " + k.idn.toString() + "\n value reanalysis: "+ v.toString + "\n value updated: " + updatedValue.toString)
                println("diff: " + checkSubsumptionSetOfComponents(v, updatedValue)))


      mappingWithUpdate.foreach((k, v) =>
        mappingWithoutUpdate.get(k) match
          case Some(updatedValue) =>
          case _ => println("missing in reanalysis: " + k.toString() + "\n update value: " + v.toString)
      )

      println()

      println()

      println("Visited with updating: " + visitedWithUpdate.toString)
      //   println("Visited with regular reanalysis: " + visitedWithoutUpdate.toString)


      println("Visited reanalysis -> Update: " + visitedWithoutUpdate.forall(e => visitedWithUpdate.contains(e)).toString)

      println("Visited update -> reanalysis: " + visitedWithUpdate.forall(e => visitedWithoutUpdate.contains(e)).toString)

      visitedWithoutUpdate.foreach(e =>
        println(e)
        e match
          case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
            println(env)
          case _ =>
        if !visitedWithUpdate.contains(e) then
          e match
            case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
              println("missing in update: " + checkSubsumptionSetOfComponents(visitedWithoutUpdate.diff(visitedWithUpdate), visitedWithUpdate))// + e.toString())
            case _ => println(e.getClass)
      )

      /*    println(visitedWithUpdate.exists(wu =>
                wu match
                  case SchemeModFComponent.Call((wlam: SchemeLambdaExp, wenv: BasicEnvironment[_]), woldCtx: maf.modular.scheme.modf.ArgContext) =>
                    //println("ctxs: ")
                   // println(woldCtx)
                   // println(oldCtx)
                    //woldCtx.values.foreach(v => print(v.hashCode() + " "))
                   // println()
                    if lam.equals(wlam) && wenv.equals(wenv) then
                      oldCtx.values.forall(v =>
                        woldCtx.values.exists(w => analysisWithUpdates.lattice.subsumes(w.asInstanceOf[analysisWithUpdates.Value], v.asInstanceOf[analysisWithUpdates.Value])))
                    else false
                  case _ => false))
            case _ =>
              )*/


      println("UPDATE")

      println(storeWithUpdate.size + " (" + storeWithoutUpdate.size + ")")
      println(depsWithUpdate.size + "  (" + depsWithoutUpdate.size + ")")
      println(mappingWithUpdate.size + " (" + mappingWithoutUpdate.size + ")")
      println(visitedWithUpdate.size + "  (" + visitedWithoutUpdate.size + ")")

      visitedWithUpdate.foreach(e =>
        if !visitedWithoutUpdate.contains(e) then
          e match
            case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: maf.modular.scheme.modf.ArgContext) =>
              println("missing in reanalysis: "))// + e.toString()))*/

      val update = new IncrementalUpdateDatastructures



      println()

    } catch {
      case e: Exception =>
      /*    e.printStackTrace(System.out)
          Writer.writeln(w, bench)
          Writer.writeln(w, e.getStackTrace().toString)
          Writer.writeln(w, "")*/
    }
  end modfAnalysis

  val modConcbenchmarks: List[String] = List()
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/testsWithUpdate/testfile.scm")
  val modFbenchmarks: List[String] = List("test/changes/scheme/nboyer.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/mixOfChanges/R5RS/gambit/array1.scm")
// val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm", "test/changeDetectionTest/onlyConsistentRenaming/Vectors.scm", "test/changeDetectionTest/onlyConsistentRenaming/Lists.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/ConRenamingLambdas.scm")
  //val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/symbols.scm")
 // val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/R5RS/various/NoSensitivity/SICP-compiler.scm")
 // val modFbenchmarks: List[String] = List("test/changeDetectionTest/onlyConsistentRenaming/R5RS/ad/mesort.scm")
  val standardTimeout: () => Timeout.T = () => Timeout.start(Duration(2, MINUTES))

  modFbenchmarks.foreach(modfAnalysis(_, standardTimeout))
  println("Done")