package aoc2024

import com.typesafe.scalalogging.Logger

/** Day16 - Reindeer Maze
  *
  * Another grid/map/maze walking challenge.
  *
  * We are looking to find the shortest, least expensive, lowest cost/lowest points path through the
  * maze. Let's call this number the score.
  *
  * My initial solution was to use dfs and bfs. This worked. I got the right solution for all test
  * cases for part1, but not for the real input.
  *
  * Therefore I decided to switch gears and use scala-graph instead of my own dfs/bfs
  * implementation. For that to work we need to model the maze as a graph. Every Position is a node
  * and every node can have up to 3 edges (the 3 possible Moves). The 3 Moves and their cost are:
  *
  *   - FORWARD: 1
  *   - ROTATE_CLOCKWISE_FORWARD: 1001
  *   - ROTATE_COUNTERCLOCKWISE_FORWARD: 1001
  *
  * A node has a Position and an Orientation. For instance an EAST node can have 3 edges:
  *
  *   - a FORWARD edge with a cost of 1 that leads to the next EAST node
  *   - a ROTATE_CLOCKWISE_FORWARD edge with a cost of 1001 that leads to the next NORTH node
  *   - a ROTATE_COUNTERCLOCKWISE_FORWARD edge with a cost of 1001 that leads to the next SOUTH node
  *
  * Every postition will create 4 nodes (4 possible orientations). But some nodes might not have any
  * edges originating from them (because they are blocked by a wall) or leading to them (because
  * there is no way to get to them with that orientation).
  *
  * Strictly speaking this is a directed graph (from Node((1,1), EAST) to Node((2,1), EAST) with a
  * cost of 1). The other way around would have a cost of 2001 (rotate twice and then move forward).
  * Means the edges is directional. Luckily we can model this by just not have an edge from
  * Node((2,1), EAST) to Node((1,1), EAST)).
  *
  * This creates a directed graph (with cycles) and weighted edges.
  *
  * Then we need to find the shortest path from the start to the end.
  */

object Day16:

  import util.Grid
  import util.WDGridGraph
  import util.DPosition
  import util.DPosition.*

  val logger: Logger = Logger(this.getClass.getName)

  object Implicits:

    given ((DPosition, DPosition) => Int) = (from, to) =>
      (from.direction, to.direction) match
        case (Direction.Up, Direction.Up)       => 1
        case (Direction.Down, Direction.Down)   => 1
        case (Direction.Left, Direction.Left)   => 1
        case (Direction.Right, Direction.Right) => 1
        case _                                  => 1001

  end Implicits

  def readFile(filename: String): Grid =
    import util.Grid.Factory.given

    Grid.fromResource(filename)
  end readFile

  /** @return the score for the least expensive path to the exit */
  def part1(grid: Grid): Int =
    import Implicits.given
    import util.WDGridGraph.cheapestPath

    require(grid.start.nonEmpty, "grid.start.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val maze = WDGridGraph.fromGrid(grid)
    val start = DPosition(grid.start.get.x, grid.start.get.y, Direction.Right)
    val ends = Direction.values.map(DPosition(grid.end.get.x, grid.end.get.y, _))
    val paths = ends.flatMap(maze.cheapestPath(start, _))
    val scores = paths.map(_.map(_._2).sum)
    scores.min
  end part1

  /** @return the solution for part2 */
  def part2(grid: Grid): Int =
    import Implicits.given

    require(grid.start.nonEmpty, "grid.start.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val maze = WDGridGraph.fromGrid(grid)
    maze.nodes.size
  end part2

end Day16
