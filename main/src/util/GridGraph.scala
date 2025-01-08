package util

import scalax.collection.edges
import scalax.collection.mutable

/** A Graph that describes a grid. Defined by a Set of Positions (Nodes/Vertexs)
  * and the neighbors (Edges) each Node has .
  */

type GridGraph = mutable.Graph[Position, edges.UnDiEdge[Position]]

object GridGraph
    extends mutable.TypedGraphFactory[Position, edges.UnDiEdge[Position]]:
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    def fromGrid(grid: Grid): GridGraph =
        // require(grid.nonEmpty, "grid.nonEmpty")
        logger.debug(s"grid: ${grid}")

        val gridEdges = grid.neighbors.flatMap:
            case (pos, neighbors) =>
                logger.debug(s"pos: ${pos}, neighbors: ${neighbors}")
                neighbors.map(neighbor => new edges.UnDiEdge(pos, neighbor))

        GridGraph.from(gridEdges)
    end fromGrid

    extension (g: GridGraph)
        def shortestPath(from: Position, to: Position): List[Position] =
            val start = g.get(from)
            val end = g.get(to)
            val path = start.shortestPathTo(end)
            
            if path.isEmpty then List()
            else path.get.nodes.map(_.outer).toList.tail
        end shortestPath
end GridGraph
