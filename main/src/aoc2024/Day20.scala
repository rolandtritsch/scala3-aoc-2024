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
  *       of them and a space behind them and that are on the (original) shortest path 
  *   - Walk the race track with the wall removed to find the shortest path (in picoseconds) ... again
  *   - Rinse and repeat
  *
  * As always ... initially I used/wasted a lot of time implementing my own dfs/bfs. At the end I
  * refactored it to use scala-graph.
  */

object Day20:
  import util.Grid
  import util.Position

  val logger: Logger = Logger(this.getClass.getName)

  /** @return the RaceTrack as a Grid */
  def readFile(filename: String): Grid =
    import util.Grid.Factory.given
    Grid.fromResource(filename)

  extension (track: Grid)

    /** @return the shortest path through the race track */
    def path: List[Position] =
      import util.GridGraph.shortestPath

      val graph = util.GridGraph.fromGrid(track)
      graph.shortestPath(track.start.get, track.end.get)
    end path

    /** @return all the possible cheats (on the path)*/
    def cheats(path: List[Position]): Set[Position] =
      track.blocked.filter: b =>
        // format: off
        (
          track.free.contains(b.up)
          && track.free.contains(b.down)
          && (path.contains(b.up) || path.contains(b.down))
        ) || (
          track.free.contains(b.left)
          && track.free.contains(b.right)
          && (path.contains(b.left) || path.contains(b.right))
        )
        // format: on
    end cheats

    /** @return all the possible grids (with a wall removed) */
    def shortCuts(cheats: Set[Position]): Set[Grid] =
      cheats.map(c => track.clone(free = track.free + c, blocked = track.blocked - c))

    /** @return how much a cheat saves in picoseconds */
    def saving(initial: Int) = initial - track.path.size

  /** @return the solution for part1 */
  def part1(track: Grid, threshold: Int = 100): Int =
    require(track.start.nonEmpty, "track.start.nonEmpty")
    require(track.end.nonEmpty, "track.end.nonEmpty")
    logger.debug(s"track: ${track}")

    val path = track.path
    val initial = path.size
    val cheats = track.cheats(path)
    val savings = track.shortCuts(cheats).toList.map(_.saving(initial))

    savings.filter(_ >= threshold).size
  end part1

  /** @return the solution for part2 */
  def part2(track: Grid): Int =
    require(track.start.nonEmpty, "track.start.nonEmpty")
    require(track.end.nonEmpty, "track.end.nonEmpty")
    logger.debug(s"track: ${track}")

    track.blocked.size
  end part2

end Day20
