package aoc2024

import com.typesafe.scalalogging.Logger

/** Day10 - Hoof It
  *
  * part1:
  *
  * Looks like another grid traversal/walking problem.
  *
  * Just wondering if there is also an angle to build combinations of digits and
  * find the (shortest) ones that start with 0 and end with 9.
  *
  * But (probably) not the way to go ...
  *
  * Let's stick with the walk the map approach ...
  *
  * Note: Let's make minimal preperations for part2 (e.g. detecting loops,
  * finding shortest path, ...) by keep the list of positions that will get us
  * to the end of the trial.
  *
  *   - walk the map to get the positions of all trail-heads
  *   - for every trail head collect all paths/positions
  *   - count the number of unique path for every trail-head (the scores) and
  *     sum them up
  *
  * part2:
  *
  * Wow. This time I did hit the jack-pot. I did guess part2 right. I already
  * had everything I need. Just implement the rating method.
  */

object Day10:
    val logger: Logger = Logger(this.getClass.getName)

    /** @return the topographical map for the give input file */
    def readFile(filename: String): TopographicalMap =
        import scala.io.Source

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try
            val lines = source.getLines().toArray
            val parsed = lines.map: line =>
                line.toArray.map(_.toString).map: height =>
                    height match
                        case "." => -99
                        case _   => height.toInt

            TopographicalMap(parsed)
        finally source.close()
        end try
    end readFile

    type Height = Int

    type Grid = Array[Array[Height]]

    case class Position(x: Int, y: Int)

    /** A map of the given topography */
    class TopographicalMap(private val grid: Grid):
        val maxX = grid.size
        val maxY = grid(0).size // scalafix:ok
        val minHeight = 0
        val maxHeight = 9

        extension (p: Position)

            /** @return true, if the position is on the grid */ // scalafix:ok
            def isOnGrid: Boolean =
                p.x >= 0 && p.x < maxX && p.y >= 0 && p.y < maxY

            /** @return all possible next positions for this position */
            private def next: Set[Position] =
                // format: off
                Set(
                    Position(p.x, p.y + 1),
                    Position(p.x + 1, p.y),
                    Position(p.x, p.y - 1),
                    Position(p.x - 1, p.y),
                )
                // format: on
                    .filter(_.isOnGrid).filter(this(_) == this(p) + 1)
            end next

            /** @return all paths for the given position/trailhead */
            def collect(ps: Set[Position]): Set[Set[Position]] =
                if this(p) == maxHeight then Set(ps + p)
                else p.next.flatMap(_.collect(ps + p))
            end collect

            /** @return the score for the given position/trailhead */
            def score: Int =
                val allTrials = p.collect(Set())
                val allTrialsWithAUniqueExit = allTrials.map(_.filter(p0 =>
                    this(p0) == minHeight || this(p0) == maxHeight
                ))
                allTrialsWithAUniqueExit.size
            end score

            /** @return the rating for the given position/trailhead */
            def rating: Int =
                val allTrials = p.collect(Set())
                allTrials.size
        end extension

        def apply(p: Position): Int = grid(p.x)(p.y)

        /** @return all trailHeads */
        def trailHeads: Set[Position] =
            val cells = grid.zipWithIndex.flatMap: (line, x) =>
                line.zipWithIndex.map((v, y) => (v, x, y))
            val heads = cells
                .withFilter: (v, _, _) =>
                    v == minHeight
                .map: (_, x, y) =>
                    Position(x, y)
            heads.toSet
        end trailHeads
    end TopographicalMap

    /** @return the sum of all scores for all trailheads */
    def part1(tp: TopographicalMap): Int =
        // require(tp.nonEmpty, "tp.nonEmpty")
        logger.debug(s"tp: ${tp}")

        tp.trailHeads.toSeq.map(tp.score(_)).sum
    end part1

    /** @return the sum of all ratings for all trailheads */
    def part2(tp: TopographicalMap): Int =
        // require(tp.nonEmpty, "tp.nonEmpty")
        logger.debug(s"tp: ${tp}")

        tp.trailHeads.toSeq.map(tp.rating(_)).sum
    end part2
end Day10
