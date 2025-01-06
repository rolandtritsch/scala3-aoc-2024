package aoc2024

/** Day04 - Ceres Search
  *
  * part1:
  *
  * Just search horizontally ...
  *
  *   - First one on the array as it is
  *   - Then on the transposed array
  *   - Then on the top-left-to-bottom-right (TLBR) array
  *   - Then on the top-right-to-bottom-left (TRBL) array
  *
  * Always search for the string and the reverse of the string.
  *
  * Sum it up.
  *
  * part2:
  *
  *   - Look for MAS (and SAM) on the diagonals (TLBR and TRBL)
  *     - Top-Left to Buttom-Right (TLBR)
  *     - Top-Right to Buttom-Left (TRBL)
  *   - Get the location/co-ordinates of the As
  *   - Intersect the TLBR A locations with the TRBL A locations
  *   - Count the number of intersections
  *
  * Done.
  */

object Day04:
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Seq[String] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        line
      }
    finally source.close()

  type Position = (Int, Int)

  object Position:
    /** move through the grid TL to BR */
    def nextTLBR(position: Position, inc: Int = 1): Position =
      (position._1 + inc, position._2 + inc)

    /** move through the grid TR to BL */
    def nextTRBL(position: Position, inc: Int = 1): Position =
      (position._1 + inc, position._2 - inc)

  extension(line: String)

    /** @return all positions the what occurs in the line */
    def findAllPos(what: String): Seq[Int] =
      val parser: scala.util.matching.Regex = what.r
      parser.findAllIn(line).matchData.map(_.start).toSeq

  extension(lines: Seq[(Position, String)])

    /** @return all positions of all what occurences on all lines */
    def findAllPos(
      what: String,
      next: (Position, Int) => Position,
    ): Seq[Position] = lines.flatMap { (position, line) =>
      line.findAllPos(what).map { offset => next(position, offset) }
    }

  extension(lines: Seq[String])

    def findAll(what: String): Int = lines.foldLeft(0) { (matchCount, line) =>
      val parser: scala.util.matching.Regex = what.r
      matchCount + parser.findAllIn(line).size
    }

    def rotate: Seq[String] = lines.map(_.toSeq).transpose.map(_.reverse)
      .map(_.mkString)

    def transposed: Seq[String] = lines.map(_.toSeq).transpose.map(_.mkString)

    def diagonalizeTLBR: Seq[(Position, String)] =
      val grid = lines.map(_.toSeq)
      val rows = grid.length
      val cols = grid(0).length

      def collectDiagonal(startPosition: Position): (Position, String) =
        def collectDiagonal(position: Position, line: String): String =
          position match
            case (row, col) if (row >= rows || col >= cols) => line
            case (row, col)                                 =>
              collectDiagonal(Position.nextTLBR(position), line + grid(row)(col))

        val line = collectDiagonal(startPosition, "")
        (startPosition, line)

      val diagonals = (0 until cols).map(col => collectDiagonal(0, col)) ++
        (1 until rows).map(row => collectDiagonal(row, 0))
      diagonals

    def diagonalizeTRBL: Seq[(Position, String)] =
      val grid = lines.map(_.toSeq)
      val rows = grid.length
      val cols = grid(0).length

      def collectDiagonal(startPosition: Position): (Position, String) =
        def collectDiagonal(position: Position, line: String): String =
          position match
            case (row, col) if (row >= rows || col < 0) => line
            case (row, col)                             =>
              collectDiagonal(Position.nextTRBL(position), line + grid(row)(col))

        val line = collectDiagonal(startPosition, "")
        (startPosition, line)

      val diagonals = (0 until rows)
        .map(row => collectDiagonal(row, cols - 1)) ++ (0 until cols - 1)
        .map(col => collectDiagonal(0, col))
      diagonals

  /** @return the number of XMASes in the puzzle/grid */
  def part1(puzzle: Seq[String]): Int =
    require(puzzle.nonEmpty, "puzzle.nonEmpty")
    logger.debug(s"puzzle: ${puzzle}")

    val what = "XMAS"

    puzzle.findAll(what) + puzzle.findAll(what.reverse) +
      puzzle.transposed.findAll(what) +
      puzzle.transposed.findAll(what.reverse) + puzzle.diagonalizeTLBR.map(_._2)
        .findAll(what) + puzzle.diagonalizeTLBR.map(_._2)
        .findAll(what.reverse) + puzzle.diagonalizeTRBL.map(_._2)
        .findAll(what) + puzzle.diagonalizeTRBL.map(_._2).findAll(what.reverse)

  /** @return the number of X-MASes in the puzzle/grid */
  def part2(puzzle: Seq[String]): Int =
    require(puzzle.nonEmpty, "puzzle.nonEmpty")
    logger.debug(s"puzzle: ${puzzle}")

    val what = "MAS"

    val positionsTLBR     = puzzle.diagonalizeTLBR
      .findAllPos(what, Position.nextTLBR) ++
      puzzle.diagonalizeTLBR.findAllPos(what.reverse, Position.nextTLBR)
    // A little bit of a hack. We know that the A is the next character
    // from the start of the string
    val positionsOfAsTLBR = positionsTLBR.map(p => Position.nextTLBR(p))

    val positionsTRBL     = puzzle.diagonalizeTRBL
      .findAllPos(what, Position.nextTRBL) ++
      puzzle.diagonalizeTRBL.findAllPos(what.reverse, Position.nextTRBL)
    val positionsOfAsTRBL = positionsTRBL.map(p => Position.nextTRBL(p))

    positionsOfAsTRBL.intersect(positionsOfAsTLBR).size
