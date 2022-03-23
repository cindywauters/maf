package maf.modular.incremental.update

import maf.core.{BasicEnvironment, Expression, Identifier, Position}
import maf.language.change.CodeVersion.{New, Old}
import maf.language.scheme.{SchemeChangePatterns, SchemeExp, SchemeLambda, SchemeLambdaExp, SchemeLetrec}
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.scheme.modf
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
    version = New
    var initialEnv: Map[String, (Identifier, SchemeLambdaExp)] = Map()
    program match
      case letrec: SchemeLetrec =>
        println("28")
        initialEnv = letrec.bindings.map(e =>
          (e._1.name, (e._1, e._2))
        ).toMap
    println(initialEnv)
    //program match
      //case let: SchemeLetrec =>
       // .eval(let.bindings.head._2)

    //println(let)
    (program, secondProgram) match
      case (old: SchemeExp, nw: SchemeExp) =>
        val time = System.nanoTime()
        val changes = finder.comparePrograms(old, nw, Some(this))
        changes.reanalyse.foreach(e => e match
          case (Some(oe), Some(ne)) => allChanges = allChanges + (oe -> ne)
          case (Some(oe), None)     => allDeletes = allDeletes.::(oe)
          case _ =>
        )
        var affectedAll = changes.reanalyse.appendedAll(changes.renamings.map(_._1)).appendedAll(changes.ifs.map(_._1._1))
        var namesVisited: List[String] = List()
        val affectedLambdas: Map[Expr, Component] = visited.collect {
          case comp@SchemeModFComponent.Call((lam: Expr, env: BasicEnvironment[_]), oldCtx: _) if lam.idn.idn.tag == Position.noTag =>
            lam.name match
              case Some(name) => namesVisited = namesVisited.::(name)
              case _ =>
            (lam, comp)
        }.toMap
        var affectedLambdasPairsIntermediate = finder.findEquivalentLambdas(affectedLambdas.keys.toList, secondProgram)
     /*   affectedLambdasPairsIntermediate.foreach(e =>
          println(e._1)
          println(e._2))*/
        println("equivanent lambdas")
        println(affectedLambdasPairsIntermediate)
        var affectedLambdasPairs: List[(Expression, Expression)] = List()
        affectedLambdasPairsIntermediate.foreach(e => e match
          case (expr: Expr, Some(other: Expression)) if expr != other && !changes.reanalyse.exists(e => e._1.contains(expr)) =>
            allChanges = allChanges + (expr -> other)
            affectedLambdasPairs = affectedLambdasPairs.::(expr, other)
          case (expr: Expr, _) =>
           // mapping.get(expr).foreach(e => addToWorkList(e._2))
            affectedLambdas.get(expr).foreach(e => e match
              case SchemeModFComponent.Call((lam: Expr, _), _)  =>
                mapping.get(lam).foreach(addToWorkList)

            )
        )
        if rename then
          this match
            case a: IncrementalModAnalysis[Expression] =>
              println("66")
              changes.ifs.foreach(e =>
                e._2.foreach(id =>
                  if !namesVisited.contains(id.name) then
                    println()
                  //  val neededLam: SchemeLambdaExp = program
                    //modf.SchemeModFComponent.call((e.))
                 //   modf.SchemeModFComponent.Call(())

                )
              )
             /* println("66")
              program match
                case letrec: SchemeLetrec => letrec.bindings.foreach(e => e._2 match
                  case lam: SchemeLambda => println(lam..name)
                  case _ =>
              )*/
       /*       changes.ifs.foreach(e =>
                if (visited.contains()e._2

              )*/
              if changes.renamings.nonEmpty || changes.ifs.nonEmpty || changes.scopeChanges.nonEmpty then
                val renamed = changes.renamings.map(e => (e._1, e._2._2))//.toSet
                update.changeDataStructures(a, List(program, secondProgram), renamed, changes.ifs, changes.scopeChanges, affectedLambdasPairs)
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
        affected.foreach(addToWorkList)
        println(workList)
       // addToWorkList(initialComponent)
        println(changes.scopeChanges)
    analyzeWithTimeout(timeout)
