package aoc2024

/** Day16 - Reindeer Maze
  *
  * Another grid/map/maze walking challenge. 
  *
  * We are looking to find the shortest, least expensive, lowest
  * cost/lowest points path through the maze. Let's call this
  * number the score.
  *
  * Not sure if we can avoid building all paths to make sure we
  * find the one with the lowest score. But we can optimize the
  * search by aborting the search as soon as the current path
  * has a higher score than the smallest one we have found so
  * far.
  *
  * (As always) We probably also need to implement a loop detection
  * (by keeping a Set of visited Positions).
  *
  * Don't think we need to model free-space this time around.
  *
  * But we need the walls. But no dimenstions (because we have an
  * outerwall).
  *
  * The Moves are FORWARD, ROTATE_CLOCKWISE, ROTATE_ANTICLOCKWISE.
  *
  * The Reindeer has a Position and an Orientation. Orientation being
  * EAST, WEST, NORTH, SOUTH.
  *
  * Was debating with myself, if I go with a different implementation
  * approach (2-dimensional array and iterators (just to learn something
  * new), but will stick with the case classes and the recursion (for
  * now).
  *
  * But I will make a slight improvement on the modeling compared to the
  * previous days. I will distinguish between the Maze and the MazeState.
  * The Maze will be the static part of the maze (walls and exit) and the
  * MazeState will be the dynamic part of the Maze (current position of
  * the Reindeer and the Moves that got us there and the score so far).
  *
  * Note: I do not think that the visited positions are part of the
  * MazeState, because we need to keep track of the visited positions
  * across all subtrees (not just within the tree).
  * 
  * Part1:
  *
  * - Read the file/maze
  * - Do a recursive tree search (every node has (up to) three children
  *   (the 3 possible Moves))
  * - Note: While we do the search lets keep track of the lowest score
  *   so far AND the path that produced the score (maybe we need that
  *   path for part2)
  * - Return the lowest score found
  */

object Day16 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  enum Move {
    case FORWARD, ROTATE_CLOCKWISE, ROTATE_ANTICLOCKWISE
  }
  import Move._

  val cost = Map(
    FORWARD -> 1,
    ROTATE_CLOCKWISE -> 1000,
    ROTATE_ANTICLOCKWISE -> 1000
  )

  extension (path: List[(Reindeer, Move)]) {
    def score: Int = path.map(p => cost(p._2)).sum
  }

  enum Orientation {
    case EAST, WEST, NORTH, SOUTH
  }
  import Orientation._

  case class Position(x: Int, y: Int) {
    def next(orienation: Orientation): Position = orienation match {
      case EAST => Position(x, y + 1)
      case WEST => Position(x, y - 1)
      case NORTH => Position(x - 1, y)
      case SOUTH => Position(x + 1, y)
    }
  }

  val rotate = Map(
    (ROTATE_CLOCKWISE, SOUTH) -> WEST,
    (ROTATE_CLOCKWISE, WEST) -> NORTH,
    (ROTATE_CLOCKWISE, NORTH) -> EAST,
    (ROTATE_CLOCKWISE, EAST) -> SOUTH,
    (ROTATE_ANTICLOCKWISE, SOUTH) -> EAST,
    (ROTATE_ANTICLOCKWISE, EAST) -> NORTH,
    (ROTATE_ANTICLOCKWISE, NORTH) -> WEST,
    (ROTATE_ANTICLOCKWISE, WEST) -> SOUTH,
  )

  case class Reindeer(val position: Position, val orientation: Orientation) {
    def step(maze: Maze, path: List[(Reindeer, Move)], visited: Map[Position, Int], bestPath: Option[List[(Reindeer, Move)]]): (Map[Position, Int], Option[List[(Reindeer, Move)]]) = {
      logger.debug(s"step: this: ${this}, path: ${path}, visited: ${visited}, bestPath: ${bestPath}")

      if (position == maze.exit) bestPath match {
        case Some(best) => if (path.score < best.score) (visited, Some(path)) else (visited, bestPath)
        case None => (visited, Some(path))
      } else if (path.score >= visited.getOrElse(this.position,Int.MaxValue)) (visited, bestPath)
      else {
        val nextForward = next(maze, visited)
        val nextClockwise = Reindeer(position, rotate(ROTATE_CLOCKWISE, orientation)).next(maze, visited)
        val nextCounterClockwise = Reindeer(position, rotate(ROTATE_ANTICLOCKWISE, orientation)).next(maze, visited)

        def v(vv: Map[Position, Int]) = (this.position, math.min(vv.getOrElse(this.position, Int.MaxValue), path.score))

        val (vForward, bpForward) = if (nextForward.isDefined) nextForward.get.step(maze, (this, FORWARD) :: path, visited + v(visited), bestPath) else (visited, bestPath)
        val (vClockwise, bpClockwise) = if (nextClockwise.isDefined) nextClockwise.get.step(maze, (this, FORWARD) :: (this, ROTATE_CLOCKWISE) :: path, visited + v(vForward), bpForward) else (vForward, bpForward)
        val (vCounterClockwise, bpCounterClockwise) = if (nextCounterClockwise.isDefined) nextCounterClockwise.get.step(maze, (this, FORWARD) :: (this, ROTATE_ANTICLOCKWISE) :: path, visited + v(vClockwise), bpClockwise) else (vClockwise, bpClockwise)
        (vCounterClockwise, bpCounterClockwise)
      }
    }

    def next(maze: Maze, visited: Map[Position, Int]): Option[Reindeer] = {
      logger.debug(s"next: this: ${this}, visited: ${visited}")

      val nextPosition = position.next(orientation)
      if (!maze.walls.contains(nextPosition) && !visited.contains(nextPosition)) {
        Some(Reindeer(nextPosition, orientation))
      } else None
    }

    def walk(maze: Maze): Int = {
      val (_, bestPath) = step(maze, List.empty, Map.empty, Option.empty[List[(Reindeer, Move)]])

      bestPath.get.score
    }
  }

  class Maze(val walls: Set[Position], val exit: Position) {
    def this() = this(Set.empty, Position(0, 0))
    def clone(
      walls: Set[Position] = this.walls,
      exit: Position = this.exit
    ): Maze = {
      Maze(walls, exit)
    }
  }

  type State = (Maze, Reindeer)

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): State = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      val (maze, reindeer) = source.getLines().toSeq.zipWithIndex.foldLeft((new Maze(), Option.empty[Reindeer])) { case (state, (line, x)) => {
        logger.debug(s"line: ${line}")
        line.zipWithIndex.foldLeft(state) { case ((maze, reindeer), (c, y)) => c match {
          case '#' => (maze.clone(walls = maze.walls + Position(x, y)), reindeer)
          case 'S' => (maze, Some(Reindeer(Position(x, y), EAST)))
          case 'E' => (maze.clone(exit = Position(x, y)), reindeer)
          case '.' => (maze, reindeer)
          case _ => throw new RuntimeException(s"Unexpected case")
        }}
      }}

      (maze, reindeer.get)
    } finally {
      source.close()
    }
  }

  /** @return the score for the least expensive path to the exit */
  def part1(state: State): Int = {
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    val (maze, reindeer) = state
    reindeer.walk(maze)
  }

  /** @return the solution for part2 */
  def part2(state: State): Int = {
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    val (maze, reindeer) = state
    maze.exit.y
  }
}
