package maf.modular.incremental.update

import maf.core.{Address, BasicEnvironment, Expression, Identifier, Position}
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambda, SchemeLambdaExp, SchemeLetrec, SchemeLettishExp}
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.scheme.{PrmAddr, modf}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.util.benchmarks.Timeout

import scala.::
import scala.collection.mutable

trait IncrementalModAnalysisWithUpdateTwoVersions[Expr <: Expression](val secondProgram: Expr) extends IncrementalModAnalysisWithUpdate[Expr]:
    var withUpdating = false
    var timeFindingChanges: Long = 0
    var timeUpdatingStructures: Long = 0
    var timeIncrementalReanalysis: Long = 0

    var equivalentLams: Map[Expression, Expression] = Map()

    def getTimes(): String =
        s"With updating refactorings: $withUpdating \nTime finding changes in program: $timeFindingChanges \nTime updating datastructures: $timeUpdatingStructures \nTime incremental reanalysis: $timeIncrementalReanalysis"

    override def updateAnalysis(timeout: Timeout.T): Unit =
        val finder = new SchemeChangePatterns
        val update = new IncrementalUpdateDatastructures
        version = New
        (program, secondProgram) match
        case (old: SchemeExp, nw: SchemeExp) =>
            val time = System.nanoTime()
            val changes = finder.comparePrograms(old, nw, Some(this), withUpdating)
            timeFindingChanges = System.nanoTime() - time
            var affectedAll = changes.reanalyse.appendedAll(changes.renamings.map(_._1)).appendedAll(changes.ifs.map(_._1)).appendedAll(changes.scopeChanges.map((k, v) => (k._1, v._1)))
            val affectedLambdas: List[Expr] = visited.collect {
                case comp@SchemeModFComponent.Call((lam: Expr, env: BasicEnvironment[_]), oldCtx: _) =>
                lam
            }.toList
            val timeLamBefore = System.nanoTime()
            val affectedLambdasPairsIntermediate = finder.findEquivalentLambdasInFunctionOfScopes(affectedLambdas)
            var affectedLambdasPairs: List[(Expression, Expression)] = List()
            var dontUpdate: List[Expression] = List()
            affectedLambdasPairsIntermediate.foreach(e => e match
                case (expr: Expr, Some(other: Expression)) if expr != other =>
                    affectedLambdasPairs = affectedLambdasPairs.::(expr, other)
                    if !withUpdating || finder.reanalyse.exists(k => k._1.contains(expr)) then
                        dontUpdate = dontUpdate.::(expr)
                case (expr: Expr, Some(other: Expr)) =>
                   // dontUpdate = dontUpdate.::(expr)
                case (expr: Expr, _) =>)
            equivalentLams = affectedLambdasPairs.toMap
            if withUpdating then
                this match
                    case a: IncrementalModAnalysis[Expression] =>
                        val renamed = changes.renamings.map(e => (e._1, e._2._2))
                        val timeBeforeU = System.nanoTime()
                        update.changeDataStructures(a, List(program, secondProgram), renamed, changes.ifs, changes.scopeChanges, affectedLambdasPairs, changes.allLexicalEnvs, dontUpdate)
                        timeUpdatingStructures = System.nanoTime() - timeBeforeU
                    affectedAll = changes.reanalyse
            else
                this match
                    case a: IncrementalModAnalysis[Expression] =>
                        val timeBeforeU = System.nanoTime()
                        update.changeDataStructures(a, List(program, secondProgram), List(), List(), Map(), affectedLambdasPairs, changes.allLexicalEnvs, dontUpdate)
                        timeUpdatingStructures = System.nanoTime() - timeBeforeU
            var enclosingLambdas: List[Expression] = List()
            var affected = affectedAll.flatMap(e => e match
                case (Some(oldExpr: Expr), Some(nwExpr: Expr)) =>
                    (mapping.getOrElse(oldExpr, Set()), mapping.getOrElse(nwExpr, Set())) match
                        case (compold, compNew) if compold.nonEmpty || compNew.nonEmpty =>
                            compold ++ compNew
                        case _ =>
                            val enclosingnew = finder.findEnclosingLambda(nwExpr)
                            val enclosingold = finder.findEnclosingLambda(oldExpr, false)
                            if enclosingnew.isDefined then
                                enclosingLambdas = enclosingLambdas.::(enclosingnew.get) // case is needed in case the change is in an if branch that is never reached (component has to be reanalysed or the mapping will be incorrect)
                            if enclosingold.isDefined then
                                enclosingLambdas = enclosingLambdas.::(enclosingold.get)
                            Set(initialComponent)
                case (oldExpr: Expr, nwExpr: Expr) =>
                    (mapping.getOrElse(oldExpr, Set()), mapping.getOrElse(nwExpr, Set())) match
                        case (compold, compNew) if compold.nonEmpty || compNew.nonEmpty  =>
                            compold ++ compNew
                        case _ =>
                            Set(initialComponent)
                case (oldExpr: Expr, None)  =>
                    mapping.get(oldExpr) match
                        case Some(compold) =>
                            compold
                        case _ =>
                            Set(initialComponent))
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
                            case _ => affected = affected ++ Set(initialComponent))
            visited.foreach(v => v match
                case SchemeModFComponent.Call((lam, env), ctx) =>
                   // var allSubs = update.findAllSubExps(lam)
                    if finder.reanalyse.exists(r => r._2.get == lam) || enclosingLambdas.contains(lam) || (!withUpdating && affectedLambdasPairs.exists((l1, l2) => l2 == lam)) then //(finder.rename.exists((exps, rest) => exps._2 == lam))) then//affectedLambdasPairs.exists((l1, l2) => lam == l2)) then //(finder.scopeChanges.exists(s => s._2._1 == lam) || finder.rename.exists(r => r._1._2 == lam))) then
                        addToWorkList(v)
                        mapping.get(lam.asInstanceOf[Expr]) match // TODO: probably optimizable by only adding if lam is in the affected list
                            case Some(comp) => addToWorkList(comp)
                            case _          =>
                case _ =>)
            mapping = mapping + (secondProgram -> Set(initialComponent))
            affected.foreach(addToWorkList)
            if finder.inserts.nonEmpty && finder.inserts.size != changes.scopeChanges.size then  // Necessary because top-level functions might have been added
                addToWorkList(initialComponent)
            if finder.deletes.nonEmpty && finder.deletes.size != changes.scopeChanges.size then  // Necessary because top-level functions might have been added
                addToWorkList(initialComponent)
       // println(workList)
        val beforeUpdateAnalysis = System.nanoTime
       // analyzeWithTimeout(timeout)
        timeIncrementalReanalysis = System.nanoTime - beforeUpdateAnalysis
