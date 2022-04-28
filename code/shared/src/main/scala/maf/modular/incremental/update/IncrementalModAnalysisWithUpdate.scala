package maf.modular.incremental.update

import maf.core.Expression
import maf.language.change.CodeVersion.New
import maf.language.scheme.{SchemeChangePatterns, SchemeExp}
import maf.modular.incremental.IncrementalModAnalysis
import maf.util.benchmarks.Timeout

trait IncrementalModAnalysisWithUpdate[Expr <: Expression] extends IncrementalModAnalysis[Expr]:

    override def updateAnalysis(timeout: Timeout.T): Unit =
        version = New // Make sure the new program version is analysed upon reanalysis (i.e., 'apply' the changes).
        val finder = new SchemeChangePatterns
        val update = new IncrementalUpdateDatastructures
        program match
            case expr: SchemeExp =>
                //println(program)
                val changedAndRenamings = finder.checkForRenamingParameter(expr).toList
                val notRenamed = changedAndRenamings.filter(e => !e._2._1)
                val renamed = changedAndRenamings.filter(e => e._2._1).map(e => (e._1, e._2._2))
                val notRenamedOld = notRenamed.map(e => e._1._1)
                (this, notRenamedOld) match
                    case (a: IncrementalModAnalysis[Expression], notRenamedOld: List[Expr]) =>
                        if renamed.nonEmpty then
                            update.changeDataStructures(a, List(program), renamed)
                        val affected = notRenamedOld.flatMap(e =>
                            mapping.get(e) match
                                case Some(comp) => comp
                                case _ => Set()
                        )
                        affected.foreach(addToWorkList)
        analyzeWithTimeout(timeout)



