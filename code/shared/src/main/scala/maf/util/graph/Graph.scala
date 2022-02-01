package maf.util.graph

sealed trait GraphMetadata:

    /** Find a specific key in this metadata, if present */
    def find(key: String): Option[GraphMetadata] = None
case class GraphMetadataMap(map: Map[String, GraphMetadata]) extends GraphMetadata:
    override def find(key: String) = map.get(key)
case class GraphMetadataString(str: String) extends GraphMetadata
case class GraphMetadataBool(bool: Boolean) extends GraphMetadata
case class GraphMetadataValue[V](v: V) extends GraphMetadata
case object GraphMetadataNone extends GraphMetadata

trait GraphElement:
    def label: String
    def color: Color
    def metadata: GraphMetadata

    /** Should  the graph element constrain  the ranking of the node during visualisation */
    def constrain: Boolean = true

class NoTransition extends GraphElement:
    def label = ""
    def color = Colors.Black
    def metadata = GraphMetadataNone

/** Defines an edge to be equal when it is from and to the same node1 otherwise same as <code>NoTransition</code> */
case class NoTransitionBetween(n1: GraphElement, n2: GraphElement) extends NoTransition

class BumpTransition extends GraphElement:
    def label = ""
    def color = Colors.Red
    def metadata = GraphMetadataNone
    override def constrain = false

object EmptyGraphElement

/**
 * A graph with nodes of type N and edges of type E. Edges have a specific type because they may contain information (i.e., they can be annotated).
 */
trait Graph[G, N <: GraphElement, E <: GraphElement]:

    /** The empty graph */
    def empty: G

    /** Add a node to the graph, without any edge */
    def addNode(g: G, node: N): G

    /** Add an edge between two nodes, and also adds the nodes that are not yet in the graph */
    def addEdge(
        g: G,
        node1: N,
        edge: E,
        node2: N
      ): G

    /** Add multiple edges at a time */
    def addEdges(g: G, l: Iterable[(N, E, N)]): G =
      l.foldLeft(g)({ case (g, (n1, e, n2)) => addEdge(g, n1, e, n2) })

    /** Remove a node from the graph */
    def removeNode(g: G, node: N): G

    /** Remove an edge between two nodes from the graph. Does not remove any node. */
    def removeEdge(
        g: G,
        node1: N,
        edge: E,
        node2: N
      ): G

    /** Returns the number of nodes in the graph */
    def nodes(g: G): Int

    /** Returns the number of edges in the graph */
    def edges(g: G): Int

    /** Finds nodes from the graph satisfying the predicate */
    def findNodes(g: G, p: N => Boolean): Set[N]

    /** Finds the node by the given id if supported */
    def findNodeById(g: G, id: Int): Option[N] = None

object Graph:
    def apply[G, N <: GraphElement, E <: GraphElement]()(implicit g: Graph[G, N, E]): Graph[G, N, E] =
      g

    implicit class GraphOps[G, N <: GraphElement, E <: GraphElement](
        g: G
      )(implicit ev: Graph[G, N, E]):
        def addNode(node: N): G = ev.addNode(g, node)
        def addEdge(
            node1: N,
            edge: E,
            node2: N
          ): G = ev.addEdge(g, node1, edge, node2)
        def addEdges(l: Iterable[(N, E, N)]): G = ev.addEdges(g, l)
        def removeNode(node: N): G = ev.removeNode(g, node)
        def removeEdge(
            node1: N,
            edge: E,
            node2: N
          ): G = ev.removeEdge(g, node1, edge, node2)
        def nodes: Int = ev.nodes(g)
        def edges: Int = ev.nodes(g)
        def findNodes(p: N => Boolean): Set[N] = ev.findNodes(g, p)
        def findNodeById(id: Int): Option[N] = ev.findNodeById(g, id)
