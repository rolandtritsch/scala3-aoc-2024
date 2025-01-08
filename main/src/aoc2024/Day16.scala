package aoc2024

/** Day16 - Reindeer Maze
  *
  * Another grid/map/maze walking challenge.
  *
  * We are looking to find the shortest, least expensive, lowest cost/lowest
  * points path through the maze. Let's call this number the score.
  *
  * Not sure if we can avoid building all paths to make sure we find the one
  * with the lowest score. But we can optimize the search by aborting the search
  * as soon as the current path has a higher score than the smallest one we have
  * found so far.
  *
  * (As always) We probably also need to implement a loop detection (by keeping
  * a Set of visited Positions).
  *
  * Don't think we need to model free-space this time around.
  *
  * But we need the walls. But no dimenstions (because we have an outerwall).
  *
  * The Moves are FORWARD, ROTATE_CLOCKWISE, ROTATE_COUNTERCLOCKWISE.
  *
  * The Reindeer has a Position and an Orientation. Orientation being EAST,
  * WEST, NORTH, SOUTH.
  *
  * Was debating with myself, if I go with a different implementation approach
  * (2-dimensional array and iterators (just to learn something new), but will
  * stick with the case classes and the recursion (for now).
  *
  * Part1:
  *
  *   - Read the file/maze
  *   - Do a recursive tree search (every node has (up to) three children (the 3
  *     possible Moves))
  *   - Note: While we do the search lets keep track of the lowest score so far
  *     AND the path that produced the score (maybe we need that path for part2)
  *   - Return the lowest score found
  */

object Day16:
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    enum Move:
        case FORWARD, ROTATE_CLOCKWISE, ROTATE_COUNTERCLOCKWISE
    import Move.*

    // format: off
    val cost = Map(
        FORWARD -> 1,
        ROTATE_CLOCKWISE -> 1000,
        ROTATE_COUNTERCLOCKWISE -> 1000,
    )
    // format: on

    extension (path: List[(Reindeer, Move)])
        def score: Int = path.map(p => cost(p._2)).sum

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

    // format: off
    val rotate = Map(
        (ROTATE_CLOCKWISE, SOUTH) -> WEST,
        (ROTATE_CLOCKWISE, WEST) -> NORTH,
        (ROTATE_CLOCKWISE, NORTH) -> EAST,
        (ROTATE_CLOCKWISE, EAST) -> SOUTH,
        (ROTATE_COUNTERCLOCKWISE, SOUTH) -> EAST,
        (ROTATE_COUNTERCLOCKWISE, EAST) -> NORTH,
        (ROTATE_COUNTERCLOCKWISE, NORTH) -> WEST,
        (ROTATE_COUNTERCLOCKWISE, WEST) -> SOUTH,
    )
    // format: on

    case class Reindeer(val position: Position, val orientation: Orientation):
        extension (bestPath: Option[List[(Reindeer, Move)]])
            def min(
                path: List[(Reindeer, Move)]
            ): Option[List[(Reindeer, Move)]] = bestPath match
                case Some(bp) =>
                    if bp.score > path.score then Some(path) else bestPath
                case None => Some(path)

            def min(
                path: Option[List[(Reindeer, Move)]]
            ): Option[List[(Reindeer, Move)]] = bestPath match
                case Some(bp) => path.min(bp)
                case None     => path
        end extension

        /** @return the shortest path through the maze */
        def dfs(
            maze: Maze,
            path: List[(Reindeer, Move)] = List.empty,
            bestPath: Option[List[(Reindeer, Move)]] = None,
            visited: Map[Position, Int] =
                Map.empty.withDefaultValue(Int.MaxValue),
        ): (Map[Position, Int], Option[List[(Reindeer, Move)]]) =
            if position == maze.exit then (visited, bestPath.min(path))
            else if path.score >= visited(this.position) then (visited, None)
            else
                val nextForward = next(maze, visited)
                val nextClockwise =
                    Reindeer(position, rotate(ROTATE_CLOCKWISE, orientation))
                        .next(maze, visited)
                val nextCounterClockwise =
                    // format: off
                    Reindeer(position, rotate(ROTATE_COUNTERCLOCKWISE, orientation))
                        .next(maze, visited)
                    // format: on
                val nextVisited = visited.updated(this.position, path.score)

                val (vForward, bpForward) =
                    if nextForward.isDefined then
                        // format: off
                        nextForward.get.dfs(
                            maze,
                            (this, FORWARD) :: path,
                            bestPath,
                            nextVisited,
                        )
                        // format: on
                    else (nextVisited, bestPath)
                val (vClockwise, bpClockwise) =
                    if nextClockwise.isDefined then
                        // format: off
                        nextClockwise.get.dfs(
                            maze,
                            (Reindeer(position, rotate(ROTATE_CLOCKWISE, orientation)), FORWARD) :: (this, ROTATE_CLOCKWISE) :: path,
                            bpForward.min(bestPath),
                            vForward
                        )
                        // format: on
                    else (vForward, bpForward.min(bestPath))
                val (vCounterClockwise, bpCounterClockwise) =
                    if nextCounterClockwise.isDefined then
                        // format: off
                        nextCounterClockwise.get.dfs(
                            maze,
                            (Reindeer(position, rotate(ROTATE_COUNTERCLOCKWISE, orientation)), FORWARD) :: (this, ROTATE_COUNTERCLOCKWISE) :: path,
                            bpClockwise.min(bpForward),
                            vClockwise,
                        )
                        // format: on
                    else (vClockwise, bpClockwise.min(bpForward))
                (vCounterClockwise, bpCounterClockwise.min(bpClockwise))
            end if
        end dfs

        /** @return the nex Reindeer or None if there is no next */
        def next(maze: Maze, visited: Map[Position, Int]): Option[Reindeer] =
            logger.debug(s"next: this: ${this}, visited: ${visited}")

            val nextPosition = position.next(orientation)
            if !maze.walls.contains(nextPosition) then
                Some(Reindeer(nextPosition, orientation))
            else None
        end next

        /** @return
          *   the score for the least expensive path to the exit (after walking
          *   the maze)
          */
        def walk(maze: Maze): Int =
            val (_, bestPath) = dfs(maze)
            bestPath.get.score
    end Reindeer

    class Maze(val walls: Set[Position], val exit: Position):
        def this() = this(Set.empty, Position(0, 0))

        def clone(
            walls: Set[Position] = this.walls,
            exit: Position = this.exit,
        ): Maze = Maze(walls, exit)
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
                            case ((maze, reindeer), (c, y)) =>
                                // format: off
                                c match
                                    case '#' => (maze.clone(walls = maze.walls + Position(x, y)), reindeer)
                                    case 'S' => (maze, Some(Reindeer(Position(x, y), EAST)))
                                    case 'E' => (maze.clone(exit = Position(x, y)), reindeer)
                                    case '.' => (maze, reindeer)
                                    case _ => throw new RuntimeException(s"Unexpected case: $c")
                                // format: on

            (maze, reindeer.get)
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
