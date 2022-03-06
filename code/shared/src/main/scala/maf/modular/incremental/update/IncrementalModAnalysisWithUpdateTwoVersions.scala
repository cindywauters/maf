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
        changes.reanalyse.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        var affectedAll = changes.reanalyse.appendedAll(changes.renamings.map(_._1)).appendedAll(changes.ifs.map(_._1._1))
        if rename then
          this match
            case a: IncrementalModAnalysis[Expression] =>
              if changes.renamings.nonEmpty || changes.ifs.nonEmpty then
                println("ifs: ")
                println(changes.renamings)
                val renamed = changes.renamings.map(e => (e._1, e._2._2)).toSet
                update.changeDataStructures(a, program, renamed, changes.ifs)
          affectedAll = changes.reanalyse
        val affected = affectedAll.flatMap(e => e match
          case (Some(old: Expr), Some(_)) =>
            mapping.get(old) match
              case Some(comp) => comp
              case _ => Set(initialComponent)
          case _ => Set(initialComponent)
        )
       // addToWorkList(initialComponent)
        println("affected: ")
        println(affectedAll)
        affected.foreach(addToWorkList)
    analyzeWithTimeout(timeout)
