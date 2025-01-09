package aoc2024

import com.typesafe.scalalogging.Logger
import util.GridGraph

/** Day18 - RAM Run
  *
  * Sounds like another dfs grid traversal problem to find the shortest path
  * from the start to the end.
  *
  * This is a complete rewrite of my initial solution.
  *
  * First ... I played with my own implementations of dfs and bfs (see util.Dfs
  * and util.Bfs). This worked. I got the right solution (for part1).
  *
  * Before tackling part2 I decided to switch gears and use scala-graph instead
  * of my own bfs/dfs implementation.
  *
  * Part1:
  *
  *   - Read the file/memory (into a grid and then into a graph)
  *   - Find and return the shortest path from the start to the end
  *
  * Part2:
  *
  *   - Read all the files/memorys (into a grid and then into a graph), one by
  *     one by one
  *   - Find and return the first one/byte that has no (shortest) path (anymore)
  */

object Day18:
    val logger: Logger = Logger(this.getClass.getName)

    import util.Grid.*
    import util.Position

    def fromResource[G](
        filename: String,
        initialGridBytes: Int = Int.MaxValue,
    )(
        using factory: GridFactory[G]
    ): (G, Seq[Position]) =
        def parseLine(line: String): Position =
            val parsed = line.split(",").map(_.toInt)
            assert(parsed.length == 2, s"parsed.size == 2: ${parsed.length}")
            Position(parsed(1), parsed(0))

        val source = scala.io.Source.fromResource(filename)
        try
            val lines = source.getLines().toSeq
            val (initialBytes, remainingBytes) = lines.splitAt(initialGridBytes)

            val blocked = initialBytes.map(parseLine).toSet

            val dimensions =
                (blocked.map(_.x).max + 1, blocked.map(_.y).max + 1)
            val (dimX, dimY) = dimensions

            val free =
                (0 until dimX).flatMap: x =>
                    (0 until dimY).map: y =>
                        Position(x, y)
                .filterNot(blocked.contains(_))
                .toSet

            // format: off
            val grid = factory.create(
                free,
                blocked,
                Some(Position(0, 0)),
                Some(Position(dimX - 1, dimY - 1)),
                dimensions,
            )
            // format: on
            val bytes = remainingBytes.map(parseLine)
            (grid, bytes)
        finally source.close()
        end try
    end fromResource

    /** @return the shortest path through the corrupted memory */
    def part1(grids: (util.Grid, Seq[util.Position])): Int =
        import util.GridGraph.shortestPath

        val (grid, _) = grids
        val memory = GridGraph.fromGrid(grid)

        memory.shortestPath(grid.start.get, grid.end.get).size
    end part1

    /** @return the first byte that has no (shortest) path (anymore) */
    def part2(grids: (util.Grid, Seq[util.Position])): String =
        import util.GridGraph.shortestPath

        def findNoPath(
            grid: util.Grid,
            remainingBytes: Seq[util.Position],
        ): Option[Position] = remainingBytes match
            case Nil => None
            case b :: bs =>
                val g =
                    grid.clone(free = grid.free - b, blocked = grid.blocked + b)
                val memory = GridGraph.fromGrid(g)
                val path = memory.shortestPath(g.start.get, g.end.get)
                if path.isEmpty then Some(b) else findNoPath(g, bs)
        end findNoPath

        val (grid, remainingBytes) = grids
        val byte = findNoPath(grid, remainingBytes).get // scalafix:ok
        (byte.y, byte.x).toString
    end part2
end Day18
