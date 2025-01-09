package aoc2024

import com.typesafe.scalalogging.Logger

/** Day20 - Race Condition
  *
  * Here we go again ... another grid traversal/walking problem. But with a twist: We are going to
  * create variations of grids to find the "best" one.
  *
  * The (initial) datastructures will will look like the datastructures from Day16.
  *
  * Part1:
  *
  *   - Read in the race track and the program
  *   - Find the shortest path (in picoseconds)
  *   - Replace a wall with a space
  *     - We do not want to remove outer walls
  *     - We (probably) do not need to keep track of the free space (we can just remove the wall)
  *     - The walls that can be removed are the ones that are on the ones that have a space in fron
  *       of them and a space behind them
  *   - Walk the race track with the wall removed
  *   - Find the shortest path (in picoseconds) ... again
  *   - Rinse and repeat
  */

object Day20:
  import scala.collection.mutable

  val logger: Logger = Logger(this.getClass.getName)

  val visited: mutable.Map[Position, Int] = mutable.Map.empty[Position, Int]
    .withDefaultValue(Int.MaxValue)

  case class Position(x: Int, y: Int):

    /** @return all possible next positions for this position */
    def next(walls: Set[Position]): Seq[Position] =
      Seq(Position(x - 1, y), Position(x + 1, y), Position(x, y - 1), Position(x, y + 1))
        .filterNot(walls.contains(_))

    /** @return the shortest path through the race track */
    def dfs(
        track: RaceTrack,
        path: List[Position] = List.empty,
        shortestPath: Option[List[Position]] = None,
    ): Option[List[Position]] =
      if this == track.end then shortestPath.min(path)
      else if path.size >= visited(this) then None
      else
        visited.update(this, path.size)
        next(track.walls).foldLeft(shortestPath): (sp, n) =>
          val nextsp = n.dfs(track, path :+ this, sp)
          if nextsp.isEmpty then sp else nextsp
      end if
    end dfs

    extension (shortestPath: Option[List[Position]])

      def min(path: List[Position]): Option[List[Position]] =
        shortestPath match
          case Some(sp) if (sp.size > path.size) => Some(path)
          case sp @ Some(_)                      => sp
          case None                              => Some(path)
        end match
      end min

    end extension

  end Position

  object Position:
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))

  import scala.math.Ordering.Implicits.*

  class RaceTrack(val walls: Set[Position], val end: Position):
    def this() = this(Set.empty, Position(0, 0))

    def clone(walls: Set[Position] = this.walls, end: Position = this.end): RaceTrack =
      RaceTrack(walls, end)

    /** @return all the possible cheats */
    def cheats(shortestPath: Set[Position]): Set[Position] =
      val Position(x, y) = walls.max
      val (maxX, maxY) = (x + 1, y + 1)
      val horizontals = (1 to maxX - 2).flatMap: x =>
        (1 to maxY - 4).map: y =>
          (Position(x, y), Position(x, y + 1), Position(x, y + 2))
      val verticals = (1 to maxY - 2).flatMap: y =>
        (1 to maxX - 4).map: x =>
          (Position(x, y), Position(x + 1, y), Position(x + 2, y))
      val cheats = (horizontals ++ verticals).toSet.filter: (p1, p2, p3) =>
        !walls.contains(p1) && walls.contains(p2) && !walls.contains(p3) &&
          (shortestPath.contains(p1) || shortestPath.contains(p3))
      cheats.map(_._2)
    end cheats

    /** @return all the possible short-cuts */
    def shortCuts(
        program: Position,
        shortestPath: List[Position],
    ): Set[(Position, Option[List[Position]])] = cheats(shortestPath.toSet).map: c =>
      val cheatingWalls = walls - c
      val cheatingTrack = clone(walls = cheatingWalls)
      Day20.visited.clear()
      val sp = program.dfs(cheatingTrack)
      (c, sp)
    end shortCuts

  end RaceTrack

  type State = (RaceTrack, Position)

  /** @return the RaceTrack and the Program (in its starting position) */
  def readFile(filename: String): State =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val (track, program) = source.getLines().toSeq.zipWithIndex
        .foldLeft((new RaceTrack(), Option.empty[Position])):
          case (state, (line, x)) =>
            logger.debug(s"line: ${line}")
            line.zipWithIndex.foldLeft(state):
              case ((track, program), (c, y)) => c match
                  case '#' => (track.clone(walls = track.walls + Position(x, y)), program)
                  case 'E' => (track.clone(end = Position(x, y)), program)
                  case 'S' => (track, Some(Position(x, y)))
                  case '.' => (track, program)
                  case _   => throw new RuntimeException("Unexpected case")

      (track, program.get) // scalafix:ok
    finally source.close()
    end try
  end readFile

  /** @return the number of short-cuts that save more than 100 picoseconds */
  def part1(state: State, threshold: Int = 100): Int =
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    val (track, program) = state

    Day20.visited.clear()
    val shortestPath = program.dfs(track)
    val shortestPathLength = shortestPath.get.length // scalafix:ok

    val shortCutsPathLength = track.shortCuts(program, shortestPath.get).map: // scalafix:ok
      (c, p) => (c, p.getOrElse(List.empty).length)

    val shortCutsValue = shortCutsPathLength.toList.map: (c, p) =>
      (c, shortestPathLength - p)

    val shortCutsGroup = shortCutsValue.groupBy(_._2).map: (cheatValue, cheatList) =>
      (cheatValue, cheatList.size)

    shortCutsGroup.count(_._1 >= threshold)
  end part1

  /** @return the solution for part2 */
  def part2(state: State): Int =
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    state._1.walls.size
  end part2

end Day20
