package maf.modular.incremental.update

import maf.core.{BasicEnvironment, Expression}
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambda, SchemeLambdaExp}
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.scheme.modf.SchemeModFComponent
import maf.util.benchmarks.Timeout

import scala.::

trait IncrementalModAnalysisWithUpdateTwoVersions[Expr <: Expression](val secondProgram: Expr) extends IncrementalModAnalysisWithUpdate[Expr]:

  var allChanges: Map[Expression, Expression] = Map()
  var allDeletes: List[Expression] = List()

  def updateAnalysis(timeout: Timeout.T, rename: Boolean): Unit =
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        val changes = SchemeChangePatterns.comparePrograms(old, nw)
        changes.reanalyse.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        var affectedAll = changes.reanalyse.appendedAll(changes.renamings.map(_._1)).appendedAll(changes.ifs.map(_._1._1))
        var affectedLambdas = visited.collect {
          case comp @ SchemeModFComponent.Call((lam: Expr, env: BasicEnvironment[_]), oldCtx: _) => (lam, comp)
        }.toList
        var affectedLambdasPairsIntermediate = SchemeChangePatterns.findEquivalentLambdas(affectedLambdas.map(_._1), secondProgram)
        var affectedLambdasPairs: List[(Expression, Expression)] = List()
        affectedLambdasPairsIntermediate.foreach(e => e match
          case (expr: Expression, Some(other: Expression)) => affectedLambdasPairs = affectedLambdasPairs.::(expr, other)
          case (expr: Expression, None) => affectedLambdas.filter(e => e == expr).foreach(e => addToWorkList(e._2))
        )
        println("31")
        affectedLambdasPairs.foreach(println)
        println("affected lambdas:")
        println(affectedLambdas)
        println(changes.reanalyse)
        if rename then
          this match
            case a: IncrementalModAnalysis[Expression] =>
              if changes.renamings.nonEmpty || changes.ifs.nonEmpty then
                val renamed = changes.renamings.map(e => (e._1, e._2._2)).toSet
                update.changeDataStructures(a, program, renamed, changes.ifs, affectedLambdasPairs)
          affectedAll = changes.reanalyse
        var affected = affectedAll.flatMap(e => e match
          case (Some(old: Expr), Some(_)) =>
            mapping.get(old) match
              case Some(comp) => comp
              case _ => Set(initialComponent)
          case _ => Set(initialComponent)
        )
       // addToWorkList(initialComponent)
        affected.foreach(addToWorkList)
    analyzeWithTimeout(timeout)
