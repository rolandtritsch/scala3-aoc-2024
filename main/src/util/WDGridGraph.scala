package util

import com.typesafe.scalalogging.Logger
import scalax.collection.edges
import scalax.collection.mutable

/** A weighted, directed Graph that describes a grid. Defined by a Set of directed Positions
  * (Nodes/Vertices) and the neighbors (Edges) each Node has (and the cost of traversing that edge).
  */

type WDGridGraph = mutable.Graph[Position, edges.labeled.WDiEdge[Position]]

object WDGridGraph extends mutable.TypedGraphFactory[Position, edges.labeled.WDiEdge[Position]]:
  val logger: Logger = Logger(this.getClass.getName)

  object Implicits:

    given ((DPosition, RPosition) => Int) = (from, to) =>
      (from.direction, to.relative) match
        case (Direction.Up, Direction.Up)       => 1
        case (Direction.Down, Direction.Down)   => 1
        case (Direction.Left, Direction.Left)   => 1
        case (Direction.Right, Direction.Right) => 1
        case _                                  => 2

  end Implicits

  def fromGrid(grid: Grid)(using cost: (DPosition, RPosition) => Int): WDGridGraph =
    // require(grid.nonEmpty, "grid.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val gridEdges = grid.neighbors.flatMap:
      case (pos, neighbors) =>
        logger.debug(s"pos: ${pos}, neighbors: ${neighbors}")
        val directedPositions = Direction.values.map(d => DPosition(pos.x, pos.y, d)).toSet
        directedPositions.flatMap: dPos =>
          neighbors.map: nPos =>
            new edges.labeled.WDiEdge(dPos.toPosition, nPos.toPosition, cost(dPos, nPos))

    WDGridGraph.from(gridEdges)
  end fromGrid

  extension (g: WDGridGraph)

    def shortestPath(from: Position, to: Position): List[Position] =
      val start = g.get(from)
      val end = g.get(to)
      val path = start.shortestPathTo(end)

      if path.isEmpty then List() else path.get.nodes.map(_.outer).toList.tail // scalafix:ok
    end shortestPath

  end extension

end WDGridGraph
