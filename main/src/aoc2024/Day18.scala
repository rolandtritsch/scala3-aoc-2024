package aoc2024

/** Day18 - RAM Run
  *
  * Sounds like another dfs grid traversal problem to find the
  * shortest path from the start to the end.
  * 
  * This is a complete rewrite of my initial solution.
  * 
  * First ... I played with my own implementations of dfs and bfs
  * (see util.Dfs and util.Bfs). This worked. I got the right 
  * solution (for part1).
  * 
  * Before tackling part2 I decided to switch gears and use
  * scala-graph instead of my own bfs/dfs implementation.
  *
  * Part1:
  * 
  * - Read the file/memory (into a grid and then into a graph)
  * - Find and return the shortest path from the start to the end 
  */

object Day18 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  import util.Grid._
  import util.Position

  def fromResource[G](filename: String, numberOfLines: Int = Int.MaxValue)(using factory: GridFactory[G]): G = {
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    val source = scala.io.Source.fromResource(filename)
    try {
      val blocked = source.getLines().toSeq.take(numberOfLines).map { line =>
        logger.debug(s"line: ${line}")
        val parsed = line.split(",").map(_.toInt)
        logger.debug(s"parsed: ${parsed}")
        assert(parsed.size == 2, s"parsed.size == 2: ${parsed.size}")
        Position(parsed(1), parsed(0))
      }.toSet

      val dimensions = (blocked.map(_.x).max + 1, blocked.map(_.y).max + 1)
      val (dimX, dimY) = dimensions

      val free = (0 until dimX).flatMap { x => {
        (0 until dimY).map { y => Position(x, y) }
      }}.filter(!blocked.contains(_)).toSet

      factory.create(free, blocked, Some(Position(0, 0)), Some(Position(dimX - 1, dimY - 1)), dimensions)
    } finally {
      source.close()
    }
  }

  /** @return the shortest path through the corrupted memory */
  def part1(grid: util.Grid): Int = {
    val memory = util.GridGraph.fromGrid(grid)
    val start = memory.get(grid.start.get)
    val end = memory.get(grid.end.get)
    val path = start.shortestPathTo(end).get.edges

    path.size
  }

  /** @return the solution for part2 */
  def part2(grid: util.Grid): Int = {
    0
  }
}
