package util

import com.typesafe.scalalogging.Logger
import scalax.collection.edges
import scalax.collection.mutable

/** A weighted, directed Graph that describes a grid. Defined by a Set of directed Positions
  * (Nodes/Vertices) and the neighbors (Edges) each Node has (and the cost of traversing that edge).
  */

type WDGridGraph = mutable.Graph[DPosition, edges.labeled.WDiEdge[DPosition]]

object WDGridGraph extends mutable.TypedGraphFactory[DPosition, edges.labeled.WDiEdge[DPosition]]:
  import util.DPosition.*

  val logger: Logger = Logger(this.getClass.getName)

  object Implicits:

    given ((DPosition, DPosition) => Int) = (from, to) =>
      (from.direction, to.direction) match
        case (Direction.Up, Direction.Up)       => 1
        case (Direction.Down, Direction.Down)   => 1
        case (Direction.Left, Direction.Left)   => 1
        case (Direction.Right, Direction.Right) => 1
        case _                                  => 2

  end Implicits

  def fromGrid(grid: Grid)(using cost: (DPosition, DPosition) => Int): WDGridGraph =
    require(grid.start.nonEmpty, "grid.start.nonEmpty")
    require(grid.end.nonEmpty, "grid.end.nonEmpty")
    logger.debug(s"grid: ${grid}")

    def opposite(dPos: DPosition, nPos: DPosition): Boolean = (dPos.direction, nPos.direction) match
      case (Direction.Up, Direction.Down)    => true
      case (Direction.Down, Direction.Up)    => true
      case (Direction.Left, Direction.Right) => true
      case (Direction.Right, Direction.Left) => true
      case _                                 => false
    end opposite

    val gridEdges = grid.neighbors.flatMap:
      case (pos, neighbors) =>
        logger.debug(s"pos: ${pos}, neighbors: ${neighbors}")
        val directedPositions = Direction.values.map(DPosition(pos.x, pos.y, _)).toSet
        directedPositions.flatMap: dPos =>
          neighbors.filterNot(opposite(dPos, _)).map: nPos =>
            logger.debug(s"dPos: ${dPos}, nPos: ${nPos}, cost: ${cost(dPos, nPos)}")
            new edges.labeled.WDiEdge(dPos, nPos, cost(dPos, nPos))

    WDGridGraph.from(gridEdges)
  end fromGrid

  extension (g: WDGridGraph)

    def cheapestPath(from: DPosition, to: DPosition): Option[List[(DPosition, Int)]] =
      // def w(e: g.EdgeT): Int = e.weight

      val start = g.get(from)
      val end = g.get(to)
      val path = start.shortestPathTo(end)

      if path.isEmpty then None
      else
        // scalafix:off
        logger.debug(s"from: ${from}, to: ${to}, path: ${path}, path.weight: ${path.get.weight}")

        val positions = path.get.nodes.map(_.outer).toList.tail
        val weights = path.get.edges.map(_.weight.toInt).toList
        Some(positions.zip(weights))
        // scalafix:on
      end if
    end cheapestPath

  end extension

end WDGridGraph
