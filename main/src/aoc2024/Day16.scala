package aoc2024

import com.typesafe.scalalogging.Logger

/** Day16 - Reindeer Maze
  *
  * Another grid/map/maze walking challenge.
  *
  * We are looking to find the shortest, least expensive, lowest cost/lowest points path through the
  * maze. Let's call this number the score.
  *
  * Not sure if we can avoid building all paths to make sure we find the one with the lowest score.
  * But we can optimize the search by aborting the search as soon as the current path has a higher
  * score than the smallest one we have found so far.
  *
  * (As always) We probably also need to implement a loop detection (by keeping a Set of visited
  * Positions).
  *
  * Don't think we need to model free-space this time around.
  *
  * But we need the walls. But no dimenstions (because we have an outerwall).
  *
  * The Moves are FORWARD, ROTATE_CLOCKWISE, ROTATE_COUNTERCLOCKWISE.
  *
  * The Reindeer has a Position and an Orientation. Orientation being EAST, WEST, NORTH, SOUTH.
  *
  * Was debating with myself, if I go with a different implementation approach (2-dimensional array
  * and iterators (just to learn something new), but will stick with the case classes and the
  * recursion (for now).
  *
  * Part1:
  *
  *   - Read the file/maze
  *   - Do a recursive tree search (every node has (up to) three children (the 3 possible Moves))
  *   - Note: While we do the search lets keep track of the lowest score so far AND the path that
  *     produced the score (maybe we need that path for part2)
  *   - Return the lowest score found
  *
  * This worked for the sample input, but not for the real input.
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
  val logger: Logger = Logger(this.getClass.getName)

  enum Move:
    case FORWARD, ROTATE_CLOCKWISE, ROTATE_COUNTERCLOCKWISE

  import Move.*

  val cost: Map[Move, Int] =
    Map(FORWARD -> 1, ROTATE_CLOCKWISE -> 1000, ROTATE_COUNTERCLOCKWISE -> 1000)

  extension (path: List[(Reindeer, Move)]) def score: Int = path.map(p => cost(p._2)).sum

  enum Orientation:
    case EAST, WEST, NORTH, SOUTH

  import Orientation.*

  case class Position(x: Int, y: Int):

    def next(orienation: Orientation): Position = orienation match
      case EAST  => Position(x, y + 1)
      case WEST  => Position(x, y - 1)
      case NORTH => Position(x - 1, y)
      case SOUTH => Position(x + 1, y)
    end next

  end Position

  val rotate: Map[(Move, Orientation), Orientation] = Map(
    (ROTATE_CLOCKWISE, SOUTH) -> WEST,
    (ROTATE_CLOCKWISE, WEST) -> NORTH,
    (ROTATE_CLOCKWISE, NORTH) -> EAST,
    (ROTATE_CLOCKWISE, EAST) -> SOUTH,
    (ROTATE_COUNTERCLOCKWISE, SOUTH) -> EAST,
    (ROTATE_COUNTERCLOCKWISE, EAST) -> NORTH,
    (ROTATE_COUNTERCLOCKWISE, NORTH) -> WEST,
    (ROTATE_COUNTERCLOCKWISE, WEST) -> SOUTH,
  )

  case class Reindeer(position: Position, orientation: Orientation):

    extension (bestPath: Option[List[(Reindeer, Move)]])

      def min(path: List[(Reindeer, Move)]): Option[List[(Reindeer, Move)]] = bestPath match
        case Some(bp) => if bp.score > path.score then Some(path) else bestPath
        case None     => Some(path)

      def min(path: Option[List[(Reindeer, Move)]]): Option[List[(Reindeer, Move)]] = bestPath match
        case Some(bp) => path.min(bp)
        case None     => path

    end extension

    /** @return the shortest path through the maze */
    def dfs(
        maze: Maze,
        path: List[(Reindeer, Move)] = List.empty,
        bestPath: Option[List[(Reindeer, Move)]] = None,
        visited: Map[Position, Int] = Map.empty.withDefaultValue(Int.MaxValue),
    ): (Map[Position, Int], Option[List[(Reindeer, Move)]]) =
      if position == maze.exit then (visited, bestPath.min(path))
      else if path.score >= visited(this.position) then (visited, None)
      else
        val nextForward = next(maze, visited)
        val nextClockwise = Reindeer(position, rotate(ROTATE_CLOCKWISE, orientation))
          .next(maze, visited)
        val nextCounterClockwise = Reindeer(position, rotate(ROTATE_COUNTERCLOCKWISE, orientation))
          .next(maze, visited)
        val nextVisited = visited.updated(this.position, path.score)

        val (vForward, bpForward) =
          if nextForward.isDefined then
            nextForward.get.dfs(maze, (this, FORWARD) :: path, bestPath, nextVisited)
          else (nextVisited, bestPath)
        val (vClockwise, bpClockwise) =
          if nextClockwise.isDefined then
            // scalafix: off
            nextClockwise.get.dfs(
              maze,
              (Reindeer(position, rotate(ROTATE_CLOCKWISE, orientation)), FORWARD) ::
                (this, ROTATE_CLOCKWISE) :: path,
              bpForward.min(bestPath),
              vForward,
            )
            // scalafix: on
          else (vForward, bpForward.min(bestPath))
        val (vCounterClockwise, bpCounterClockwise) =
          if nextCounterClockwise.isDefined then
            // scalafix: off
            nextCounterClockwise.get.dfs(
              maze,
              (Reindeer(position, rotate(ROTATE_COUNTERCLOCKWISE, orientation)), FORWARD) ::
                (this, ROTATE_COUNTERCLOCKWISE) :: path,
              bpClockwise.min(bpForward),
              vClockwise,
            )
            // scalafix: on
          else (vClockwise, bpClockwise.min(bpForward))
        (vCounterClockwise, bpCounterClockwise.min(bpClockwise))
      end if
    end dfs

    /** @return the nex Reindeer or None if there is no next */
    def next(maze: Maze, visited: Map[Position, Int]): Option[Reindeer] =
      logger.debug(s"next: this: ${this}, visited: ${visited}")

      val nextPosition = position.next(orientation)
      Option.when(!maze.walls.contains(nextPosition))(Reindeer(nextPosition, orientation))
    end next

    /** @return
      *   the score for the least expensive path to the exit (after walking the maze)
      */
    def walk(maze: Maze): Int =
      val (_, bestPath) = dfs(maze)
      bestPath.get.score // scalafix:ok

  end Reindeer

  class Maze(val walls: Set[Position], val exit: Position):
    def this() = this(Set.empty, Position(0, 0))

    def clone(walls: Set[Position] = this.walls, exit: Position = this.exit): Maze =
      Maze(walls, exit)

  end Maze

  type State = (Maze, Reindeer)

  /** @return the Maze and the Reindeer (in its starting position) */
  def readFile(filename: String): State =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val (maze, reindeer) = source.getLines().toSeq.zipWithIndex
        .foldLeft((new Maze(), Option.empty[Reindeer])):
          case (state, (line, x)) =>
            logger.debug(s"line: ${line}")
            line.zipWithIndex.foldLeft(state):
              case ((maze, reindeer), (c, y)) => c match
                  case '#' => (maze.clone(walls = maze.walls + Position(x, y)), reindeer)
                  case 'S' => (maze, Some(Reindeer(Position(x, y), EAST)))
                  case 'E' => (maze.clone(exit = Position(x, y)), reindeer)
                  case '.' => (maze, reindeer)
                  case _   => throw new RuntimeException(s"Unexpected case: $c")

      (maze, reindeer.get) // scalafix:ok

    finally source.close()
    end try
  end readFile

  /** @return the score for the least expensive path to the exit */
  def part1(state: State): Int =
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    val (maze, reindeer) = state
    reindeer.walk(maze)
  end part1

  /** @return the solution for part2 */
  def part2(state: State): Int =
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    val (maze, reindeer) = state
    maze.exit.y
  end part2

end Day16
