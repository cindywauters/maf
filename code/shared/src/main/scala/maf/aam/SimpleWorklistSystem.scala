package maf.aam

import maf.util.graph.*
import scala.collection.immutable.HashSet

trait BaseSimpleWorklistSystem extends AAMAnalysis:
    trait SeenStateSystem extends BaseSystem:
        this: System =>
        var seen: HashSet[Conf] = HashSet()
        var work: List[(Option[Conf], Conf)] = List()
        var newWork: List[(Option[Conf], Conf)] = List()

        def popWork(): Option[(Option[Conf], Conf)] =
          if work.nonEmpty then
              change {
                val conf = work.head
                work = work.tail
                Some(conf)
              }
          else if newWork.nonEmpty then
              change {
                val conf = newWork.head
                // return work from new work and swap work and new work
                val keep = work
                work = newWork.tail
                newWork = keep
                Some(conf)
              }
          else None

        def pushWork(prev: Option[Conf], work: Conf): this.type = change {
          newWork = (prev, work) :: newWork
          this
        }

        def addSeen(work: Conf): Unit = change {
          seen = seen + work
        }

        def allConfs: Set[Conf] = seen

        def finalStates: Set[State] =
          seen.map(asState(_, this)).filter(isFinal)

    type System <: SeenStateSystem

    protected def integrate(st: State, sys: System): (Conf, System) =
      (asConf(st, sys), sys)

    protected def decideSuccessors[G](depGraph: G, prev: Conf, successors: Set[State], sys: System)(using g: AAMGraph[G]): (System, G) =
        var depGraph2 = depGraph
        var sys2 = sys
        successors.foreach { successor =>
            val (conf, sys1) = integrate(successor, sys)
            if !sys1.seen.contains(conf) then
                //println(s"prev $prev, succ $conf")
                sys1.pushWork(Some(prev), conf)
                sys1.addSeen(conf)
            else
                //println(s"already seen $prev $conf")
                val n1 = asGraphElement(prev, sys1)
                val n2 = asGraphElement(conf, sys1)
                depGraph2 = g.addEdge(depGraph2, n1, NoTransition(), n2)

            sys2 = sys1
        }

        (sys2, depGraph2)

    override protected def transition[G](system: System, dependencyGraph: G)(using g: AAMGraph[G]): (System, G) =
        //println(s"seen ${system.seen.size}, work ${system.work.size}, newWork ${system.newWork.size}")
        val conf = system.popWork()
        // no more work; reached fixed point
        if conf.isEmpty then (system, dependencyGraph)
        else
            // add an edge in the graph about the work
            val fdpg = if conf.get._1.nonEmpty then
                val n1 = asGraphElement(conf.get._1.get, system)
                val n2 = asGraphElement(conf.get._2, system)
                val dpg2 = g.addNode(dependencyGraph, n2)
                g.addEdge(dpg2, n1, NoTransition(), n2)
            else dependencyGraph

            // candidate successors
            val successors = step(asState(conf.get._2, system))
            decideSuccessors(fdpg, conf.get._2, successors, system)

trait SimpleWorklistSystem extends BaseSimpleWorklistSystem:
    type System = SeenStateSystem
    override def inject(expr: Expr): System =
      new SeenStateSystem {}.pushWork(None, injectConf(expr))
