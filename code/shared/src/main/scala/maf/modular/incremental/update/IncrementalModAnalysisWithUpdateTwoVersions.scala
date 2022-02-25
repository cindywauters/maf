package maf.modular.incremental.update

import maf.core.Expression
import maf.language.change.CodeVersion.{Old, New}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp}
import maf.modular.incremental.IncrementalModAnalysis
import maf.util.benchmarks.Timeout

trait IncrementalModAnalysisWithUpdateTwoVersions[Expr <: Expression](val secondProgram: Expr) extends IncrementalModAnalysisWithUpdate[Expr]:

  override def updateAnalysis(timeout: Timeout.T): Unit =
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        SchemeChangePatterns.comparePrograms(old, nw)
    addToWorkList(initialComponent)
    analyzeWithTimeout(timeout)
