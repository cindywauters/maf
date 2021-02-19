package maf.web

import maf.modular._
import maf.modular.scheme.PrmAddr
import maf.web.AddressVisualisation._
import maf.web.WebVisualisation.JsAny

import scala.scalajs.js

object AddressVisualisation {
  val __CSS_ADDRESS_NODE__ = "address_node"
  val __CSS_READ_EDGE__ = "read_edge"
  val __SVG_READ_ARROW__ = "readarrow"
}

/** Adds the visualisation of addresses to the web visualisation: visualises the addresses read by a component. */
trait AddressVisualisation extends WebVisualisation {

  var adrNodesColl: Map[analysis.Addr, AddrNode] = Map()
  var oldReadDeps: Map[Dependency, Set[analysis.Component]] = Map()

  class AddrNode(val address: analysis.Addr) extends Node {
    def displayText(): String = s"${address.toString} [${analysis.store.getOrElse(address, analysis.lattice.bottom).toString.take(10)}]"

    def data(): Any = address
  }

  def getNode(addr: analysis.Addr): AddrNode = adrNodesColl.get(addr) match {
    case None =>
      val newNode = new AddrNode(addr)
      adrNodesColl += (addr -> newNode)
      newNode
    case Some(existingNode) => existingNode
  }

  override def refreshData(): Unit = {
    super.refreshData()
    // Also add nodes for the addresses.
    analysis.store.keySet.foreach { addr =>
      if (!addr.isInstanceOf[PrmAddr]) {
        val node = getNode(addr)
        nodesData += node
        // Using the standard Global Store, we can only visualise read dependencies.
        val readers: Set[analysis.Component] = analysis.deps(AddrDependency(addr))
        readers.foreach { reader =>
          val readerNode = getNode(reader)
          val edge = getEdge(node, readerNode) // Edge from addr -> reader.
          edgesData += edge
        }
      }
    }
  }

  def deleteOnStep(component: analysis.Component): Unit

  override def refreshDataAfterStep(cmp: analysis.Component, oldCmpDeps: Set[analysis.Component]): Unit = {
    super.refreshDataAfterStep(cmp, oldCmpDeps)
    val readerNode = getNode(cmp)
    // Remove old edges (and possiblye nodes).
    deleteOnStep(cmp)
    // Add the new edges.
    analysis.deps.filter(_._2.contains(cmp)).keySet.foreach {
      case AddrDependency(addr) =>
        val addrNode: AddrNode = getNode(addr)
        val edge = getEdge(addrNode, readerNode)
        nodesData += addrNode
        edgesData += edge
      case _ =>
    }
    oldReadDeps = analysis.deps // Avoid having to override stepAnalysis etc. by saving the new state of the dependencies.
  }

  override def classifyNodes(): Unit = {
    super.classifyNodes()
    nodes.classed(__CSS_ADDRESS_NODE__,
                  (node: Node) =>
                    node match {
                      case _: AddrNode => true;
                      case _           => false
                    }
    )
  }

  override def classifyEdges(): Unit = {
    super.classifyEdges()
    edges.classed(__CSS_READ_EDGE__, (edge: Edge) => edge.source.isInstanceOf[AddrNode] && edge.target.isInstanceOf[CmpNode])
  }

  override def setupMarker(svg: JsAny): js.Dynamic = {
    super.setupMarker(svg)
    newMarker(svg, __SVG_READ_ARROW__).attr("fill", "tan").attr("stroke", "tan")
  }
}

/** Allows selecting which address nodes (and corresponding edges) are deleted upon a step in the analysis. */
trait AddressRetentionPolicy {
  this: AddressVisualisation =>

  /** Allows selecting which address nodes (and corresponding edges) are deleted upon a step in the analysis. */
  def deleteOnStep(cmp: analysis.Component): Unit
}

// Ensures all edges and addresses remain shown.
trait RetainAll extends AddressRetentionPolicy {
  this: AddressVisualisation =>
  def deleteOnStep(cmp: analysis.Component): Unit = {
    val readerNode = getNode(cmp)
    oldReadDeps.filter(_._2.contains(cmp)).keySet.foreach {
      case AddrDependency(addr) =>
        val addrNode: AddrNode = getNode(addr)
        val edge = getEdge(addrNode, readerNode)
        edgesData -= edge
      case _ =>
    }
  }
}

// Only shows the addresses read by the component last analysed.
trait RetainUpdated extends AddressRetentionPolicy {
  this: AddressVisualisation =>
  def deleteOnStep(cmp: analysis.Component): Unit = {
    nodesData = nodesData.filterNot(_.isInstanceOf[AddrNode])
    edgesData = edgesData.filterNot(_.source.isInstanceOf[AddrNode])
  }
}
