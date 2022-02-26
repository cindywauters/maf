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

  override def updateAnalysis(timeout: Timeout.T): Unit =
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        val changes = SchemeChangePatterns.comparePrograms(old, nw)
        changes._1.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        this match
          case a: IncrementalModAnalysis[Expression] =>
            if changes._2.nonEmpty then
              val renamed = changes._2.map(e => (e._1, e._2._2)).toSet
              println("renamed: ")
              println(renamed.size)
              println(changes._1)
              update.changeDataStructures(a, program, renamed)
        val affected = changes._1.flatMap(e => e match
          case (Some(old: Expr), _) =>
            mapping.get(old) match
              case Some(comp) => comp
              case _ => Set()
          case _ => Set()
        )
    //   affected.foreach(addToWorkList)
        println("41")
        println(affected)
        affected.foreach(addToWorkList)
    analyzeWithTimeout(timeout)
