package maf.modular.incremental.update

import maf.core.{BasicEnvironment, Expression, Position}
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambda, SchemeLambdaExp}
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.scheme.modf.SchemeModFComponent
import maf.util.benchmarks.Timeout

import scala.::

trait IncrementalModAnalysisWithUpdateTwoVersions[Expr <: Expression](val secondProgram: Expr) extends IncrementalModAnalysisWithUpdate[Expr]:

  var allChanges: Map[Expression, Expression] = Map()
  var allDeletes: List[Expression] = List()

  def getMapping(expr: Expression): Set[Component] =
    mapping.get(expr.asInstanceOf[Expr]) match
      case Some(comp) => comp
      case None       => Set()


  def updateAnalysis(timeout: Timeout.T, rename: Boolean): Unit =
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        val changes = SchemeChangePatterns.comparePrograms(old, nw, Some(this))
        changes.reanalyse.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        var affectedAll = changes.reanalyse.appendedAll(changes.renamings.map(_._1)).appendedAll(changes.ifs.map(_._1._1))
        var affectedLambdas = visited.collect {
          case comp @ SchemeModFComponent.Call((lam: Expr, env: BasicEnvironment[_]), oldCtx: _) if lam.idn.idn.tag == Position.noTag=>
            (lam, comp)

        }.toList
        var affectedLambdasPairsIntermediate = SchemeChangePatterns.findEquivalentLambdas(affectedLambdas.map(_._1), secondProgram)
     /*   affectedLambdasPairsIntermediate.foreach(e =>
          println(e._1)
          println(e._2))*/
        var affectedLambdasPairs: List[(Expression, Expression)] = List()
        affectedLambdasPairsIntermediate.foreach(e => e match
          case (expr: Expression, Some(other: Expression)) if expr != other && !changes.reanalyse.exists(e => e._1.contains(expr)) =>
            affectedLambdasPairs = affectedLambdasPairs.::(expr, other)
          case (expr: Expression, _) =>
           // affectedLambdas.filter(e => e._1 == expr).foreach(e => addToWorkList(e._2))
        )
        if rename then
          this match
            case a: IncrementalModAnalysis[Expression] =>
              if changes.renamings.nonEmpty || changes.ifs.nonEmpty || changes.scopeChanges.nonEmpty then
                val renamed = changes.renamings.map(e => (e._1, e._2._2))//.toSet
                update.changeDataStructures(a, program, renamed, changes.ifs, changes.scopeChanges, affectedLambdasPairs)
          affectedAll = changes.reanalyse
        var affected = affectedAll.flatMap(e => e match
          case (Some(old: Expr), Some(nw: Expr)) =>
            (mapping.get(old), mapping.get(nw)) match
              case (Some(compold), _) =>
                compold
              case _ =>
                Set(initialComponent)
          case _ => Set(initialComponent)
        )
        mapping = mapping + (secondProgram -> Set(initialComponent))
       // affected.foreach(addToWorkList)
        println(changes.scopeChanges)
    analyzeWithTimeout(timeout)
