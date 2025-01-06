package aoc2024

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
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Height = Int

  type Grid = Array[Array[Height]]

  case class Position(val x: Int, val y: Int)

  /** A map of the given topography */
  class TopographicalMap(private val grid: Grid):
    val maxX = grid.size
    val maxY = grid(0).size

    extension(p: Position)
      def isOnGrid: Boolean = p.x >= 0 && p.x < maxX && p.y >= 0 && p.y < maxY

      /** @return all possible next positions for this position */
      private def next: Set[Position] = Set(
        Position(p.x, p.y + 1),
        Position(p.x + 1, p.y),
        Position(p.x, p.y - 1),
        Position(p.x - 1, p.y),
      ).filter(_.isOnGrid).filter(this(_) == this(p) + 1)

      /** @return all paths for the given position/trailhead */
      def collect(ps: Set[Position]): Set[Set[Position]] =
        if this(p) == 9 then Set(ps + p)
        else p.next.flatMap { nextPosition => nextPosition.collect(ps + p) }

      /** @return the score for the given position/trailhead */
      def score: Int =
        val allTrials                = p.collect(Set())
        val allTrialsWithAUniqueExit = allTrials.map { t =>
          t.filter(p0 => this(p0) == 0 || this(p0) == 9)
        }
        allTrialsWithAUniqueExit.size

      /** @return the rating for the given position/trailhead */
      def rating: Int =
        val allTrials = p.collect(Set())
        allTrials.size

    def apply(p: Position): Int = grid(p.x)(p.y)

    /** @return all trailHeads */
    def trailHeads: Set[Position] =
      val cells = grid.zipWithIndex.flatMap { (line, x) =>
        line.zipWithIndex.map { (v, y) => (v, x, y) }
      }
      cells.filter((v, _, _) => v == 0).map((_, x, y) => Position(x, y)).toSet

  /** @return the topographical map for the give input file */
  def readFile(filename: String): TopographicalMap =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val lines  = source.getLines().toSeq
      val parsed = lines.map { line =>
        line.map(_.toString).map { s =>
          s match
            case "." => -99
            case _   => s.toInt
        }.toArray
      }.toArray

      TopographicalMap(parsed)
    finally source.close()

  /** @return the sum of all scores for all trailheads */
  def part1(tp: TopographicalMap): Int =
    // require(tp.nonEmpty, "tp.nonEmpty")
    logger.debug(s"tp: ${tp}")

    tp.trailHeads.toSeq.map(tp.score(_)).sum

  /** @return the sum of all ratings for all trailheads */
  def part2(tp: TopographicalMap): Int =
    // require(tp.nonEmpty, "tp.nonEmpty")
    logger.debug(s"tp: ${tp}")

    tp.trailHeads.toSeq.map(tp.rating(_)).sum
