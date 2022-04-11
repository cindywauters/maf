package maf.modular.incremental.update

import maf.core.{Address, BasicEnvironment, Expression, Identifier, Position}
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambda, SchemeLambdaExp, SchemeLetrec}
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.scheme.{PrmAddr, modf}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.util.benchmarks.Timeout

import scala.::
import scala.collection.mutable

trait IncrementalModAnalysisWithUpdateTwoVersions[Expr <: Expression](val secondProgram: Expr) extends IncrementalModAnalysisWithUpdate[Expr]:

  var allChanges: Map[Expression, Expression] = Map()
  var allDeletes: List[Expression] = List()

  def getMapping(expr: Expression): Set[Component] =
    mapping.get(expr.asInstanceOf[Expr]) match
      case Some(comp) => comp
      case None       => Set()


  def updateAnalysis(timeout: Timeout.T, rename: Boolean): Unit =
    version = New
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        val time = System.nanoTime()
        val changes = finder.comparePrograms(old, nw, Some(this))
        changes.reanalyse.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        var affectedAll = changes.reanalyse.appendedAll(changes.renamings.map(_._1)).appendedAll(changes.ifs.map(_._1._1)).appendedAll(changes.scopeChanges.map((k, v) => (k._1, v._1)))
        var namesVisited: List[String] = List()
        val affectedLambdas: List[Expr] = visited.collect {
          case comp@SchemeModFComponent.Call((lam: Expr, env: BasicEnvironment[_]), oldCtx: _) if lam.idn.idn.tag == Position.noTag =>
            lam.name match
              case Some(name) => namesVisited = namesVisited.::(name)
              case _ =>
            lam
        }.toList
        visited.foreach(v => v match
          case comp@SchemeModFComponent.Call((lam: Expr, env: BasicEnvironment[_]), oldCtx: _) if lam.idn.idn.tag != Position.noTag =>
            lam.name match
              case Some(name) => namesVisited = namesVisited.::(name)
              case _ =>
          case _ =>
        )
        val timeLamBefore = System.nanoTime()
        var affectedLambdasPairsIntermediate = finder.findEquivalentLambdasInFunctionOfScopes(affectedLambdas)
        println("time finding Lambdas:  " + (System.nanoTime() - timeLamBefore).toString)
        var affectedLambdasPairs: List[(Expression, Expression)] = List()
        var componentsWithAddedNots: List[SchemeModFComponent] = List()
        var componentsWithAddedBigger: List[SchemeModFComponent] = List()
        var dontUpdate: List[Expression] = List()
        affectedLambdasPairsIntermediate.foreach(e => e match
          case (expr: Expr, Some(other: Expression)) if expr != other =>
            allChanges = allChanges + (expr -> other)
            affectedLambdasPairs = affectedLambdasPairs.::(expr, other)
            println("affected lambda")
            println(expr)
            println(finder.reanalyse)
            if !rename || finder.reanalyse.exists(k => k._1.contains(expr)) then
              println("ADDING")
              println(expr)
              dontUpdate = dontUpdate.::(expr)
          case (expr: Expr, Some(other: Expr)) =>
            dontUpdate = dontUpdate.::(expr)

           /* if !expr.fv.contains("not") && other.fv.contains("not") then
              affectedLambdas.get(expr) match
                case Some(comp) => componentsWithAddedNots = componentsWithAddedNots.::(comp.asInstanceOf[SchemeModFComponent])
                case _          =>
            if !expr.fv.contains(">") && other.fv.contains(">") then
              affectedLambdas.get(expr) match
                case Some(comp) => componentsWithAddedBigger = componentsWithAddedBigger.::(comp.asInstanceOf[SchemeModFComponent]) */
          case (expr: Expr, _) =>
           /* affectedLambdas.get(expr).foreach(e => e match
              case SchemeModFComponent.Call((lam: Expr, _), _)  =>
                mapping.get(lam).foreach(addToWorkList)
            )*/

           // )
        )
        if rename then
          this match
            case a: IncrementalModAnalysis[Expression] =>
              /*changes.ifs.foreach(e =>
                e._2.foreach(id =>
                  if !namesVisited.contains(id.name) then
                    initialEnv.get(id.name) match
                      case Some((id: Identifier, lam: SchemeLambdaExp)) =>
                        val newEnv = BasicEnvironment[Address](lam.fv.map(fv =>
                          initialEnv.get(fv) match
                            case Some((idfv: Identifier, _)) =>
                              (fv, maf.modular.scheme.VarAddr(idfv, None))
                            case _ =>
                              (fv, PrmAddr(fv))
                        ).toMap)
                        val newComponent = SchemeModFComponent.Call((lam, newEnv), NoContext)
                        println(initialEnv.get(id.name))
                      case _ =>
                )
              )*/
              val renamed = changes.renamings.map(e => (e._1, e._2._2))//.toSet
              val timeBeforeU = System.nanoTime()
              update.changeDataStructures(a, List(program, secondProgram), renamed, changes.ifs, changes.scopeChanges, affectedLambdasPairs, changes.allLexicalEnvs, dontUpdate)
              println("time updating datastructures: " + (System.nanoTime() - timeBeforeU).toString)
          affectedAll = changes.reanalyse
        else
          this match
            case a: IncrementalModAnalysis[Expression] =>
              update.changeDataStructures(a, List(program, secondProgram), List(), List(), Map(), affectedLambdasPairs, changes.allLexicalEnvs, dontUpdate)
        var affected = affectedAll.flatMap(e => e match
          case (Some(oldExpr: Expr), Some(nwExpr: Expr)) =>
            (mapping.getOrElse(oldExpr, Set()), mapping.getOrElse(nwExpr, Set())) match
              case (compold, compNew) if compold.nonEmpty || compNew.nonEmpty =>
                println("121")
                println(oldExpr)
                println(nwExpr)
                println(compold)
                println(compNew)
                compold ++ compNew
              case _ =>
                println("128")
                println(oldExpr)
                println(nwExpr)
                Set(initialComponent)
          case (oldExpr: Expr, nwExpr: Expr) =>
            (mapping.getOrElse(oldExpr, Set()), mapping.getOrElse(nwExpr, Set())) match
              case (compold, compNew) if compold.nonEmpty || compNew.nonEmpty  =>
                println("135")
                println(oldExpr)
                println(nwExpr)
                println(compold)
                println(compNew)
                compold ++ compNew
              case _ =>
                println("142")
                println(oldExpr)
                println(nwExpr)
                Set(initialComponent)
          case (oldExpr: Expr, None)  =>
            mapping.get(oldExpr) match
              case Some(compold) =>
                compold
              case _ =>
                Set(initialComponent)
          /*case _ =>
            Set(initialComponent)*/
        )
        finder.scopeChanges.foreach((k, v) =>
          k._1 match
            case lam: SchemeLambdaExp =>
              mapping.get(lam.asInstanceOf[Expr]) match
                case Some(comps) => affected = affected ++ comps
                case _ => affected = affected ++ Set(initialComponent)
          v._1 match
            case lam: SchemeLambdaExp =>
              mapping.get(lam.asInstanceOf[Expr]) match
                case Some(comps) => affected = affected ++ comps
                case _ => affected = affected ++ Set(initialComponent)
        )
        visited.foreach(v => v match
          case SchemeModFComponent.Call((lam, env), ctx) =>
            var allSubs = update.findAllSubExps(lam)
          /*  println("158")
            println(lam)
            println(allSubs.exists(e => finder.reanalyse.exists(r => r._2.get == e) || (!rename && (finder.scopeChanges.exists(s => s._2._1 == e) && !finder.rename.exists(r => r._1._2 == e)))))
            if allSubs.exists(e => if affectedLambdasPairs.exists((l1, l2) => l2 == e) then (!rename || (!finder.scopeChanges.exists(s => s._2._1 == e) && !finder.rename.exists(r => r._1._2 == e))) else true) then*/
           // if (!rename && affectedLambdasPairs.exists((l1, l2) => l2 == lam)) ||  (affectedLambdasPairs.exists((l1, l2) => l2 == lam) && (!finder.scopeChanges.exists(e => e._2._1 == lam) && !finder.rename.exists(e => e._1._2 == lam))) then
              if finder.reanalyse.exists(r => r._2.get == lam) || (!rename && finder.rename.exists((exps, rest) => exps._2 == lam)) then//affectedLambdasPairs.exists((l1, l2) => lam == l2)) then //(finder.scopeChanges.exists(s => s._2._1 == lam) || finder.rename.exists(r => r._1._2 == lam))) then
              addToWorkList(v)
              mapping.get(lam.asInstanceOf[Expr]) match // TODO: probably optimizable by only adding if lam is in the affected list
                case Some(comp) => addToWorkList(comp)
                case _          =>
          case _ =>)
        mapping = mapping + (secondProgram -> Set(initialComponent))
        affected.foreach(addToWorkList)
        println(workList)
        println(changes.scopeChanges)
      //  addToWorkList(initialComponent)
    val beforeUpdateAnalysis = System.nanoTime
    analyzeWithTimeout(timeout)
    println("time analysis in 160: " + (System.nanoTime() - beforeUpdateAnalysis).toString)
