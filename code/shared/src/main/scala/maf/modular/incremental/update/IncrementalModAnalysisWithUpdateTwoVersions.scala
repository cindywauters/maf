package maf.modular.incremental.update

import maf.core.Expression
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambda}
import maf.modular.incremental.IncrementalModAnalysis
import maf.util.benchmarks.Timeout

import scala.::

trait IncrementalModAnalysisWithUpdateTwoVersions[Expr <: Expression](val secondProgram: Expr) extends IncrementalModAnalysisWithUpdate[Expr]:

  var allChanges: Map[Expression, Expression] = Map()
  var allDeletes: List[Expression] = List()

  def updateAnalysis(timeout: Timeout.T, rename: Boolean): Unit =
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        val changes = SchemeChangePatterns.comparePrograms(old, nw)
        changes._1.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        var affectedAll = changes._1.appendedAll(changes._2.map(_._1))
        if rename then
          this match
            case a: IncrementalModAnalysis[Expression] =>
              if changes._2.nonEmpty then
                val renamed = changes._2.map(e => (e._1, e._2._2)).toSet
                update.changeDataStructures(a, program, renamed)
          affectedAll = changes._1
        val affected = affectedAll.flatMap(e => e match
          case (Some(old: Expr), Some(_)) =>
            mapping.get(old) match
              case Some(comp) => comp
              case _ => Set()
          case _ => Set(initialComponent)
        )
        affected.foreach(addToWorkList)
    analyzeWithTimeout(timeout)
