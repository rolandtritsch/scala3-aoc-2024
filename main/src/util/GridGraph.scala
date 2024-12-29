package util

import scalax.collection.mutable
import scalax.collection.edges

/** A Graph that describes a grid. Defined by a Set of Positions (Nodes/Vertexs) and the
  * neighbors (Edges) each Node has .
  */

type GridGraph = mutable.Graph[Position, edges.UnDiEdge[Position]]
object GridGraph extends mutable.TypedGraphFactory[Position, edges.UnDiEdge[Position]] {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  def fromGrid(grid: Grid): GridGraph = {
    //require(grid.nonEmpty, "grid.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val gridEdges = grid.neighbors.map { case (pos, neighbors) => {
      logger.debug(s"pos: ${pos}, neighbors: ${neighbors}")
      neighbors.map { neighbor => new edges.UnDiEdge(pos, neighbor) }
    }}.flatten

    GridGraph.from(gridEdges)
  }
}
