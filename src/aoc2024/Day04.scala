package aoc2024

/** Day04 - Ceres Search
  *
  * part1:
  * 
  * Just search horizontally ...
  *
  * - First one on the array as it is
  * - Then on the transposed array
  * - Then on the top-left-to-bottom-right (TLBR) array
  * - Then on the top-right-to-bottom-left (TRBL) array
  *
  * Always search for the string and the reverse of the string.
  *
  * Sum it up.
  *
  * part2:
  *
  * - Look for MAS (and SAM) on the diagonals (TLBR and TRBL)
  * - Get the location/co-ordinates of the As
  * - Intersect the TLBR A locations with the TRBL A locations
  * - Count the number of intersections
  *
  *  Done.
  */

object Day04 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)


  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Seq[String] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        line
      }
    } finally {
      source.close()
    }
  }

  type Position = (Int, Int)

  extension (line: String) {
    def findAllPos(what: String): Seq[Int] = {
      val parser: scala.util.matching.Regex = what.r
      parser.findAllIn(line).matchData.map(_.start).toSeq
    }
  }

  extension (lines: Seq[(Position, String)]) {
    def findAllPosTLBR(what: String): Seq[Position] = {
      lines.flatMap { case ((x, y), line) => {
        line.findAllPos(what).map { offset => {
          (x + offset + 1, y + offset + 1)
        }}
      }}
    }

    def findAllPosTRBL(what: String): Seq[Position] = {
      lines.flatMap { case ((x, y), line) => {
        line.findAllPos(what).map { offset => {
          (x + offset + 1, y - offset - 1)
        }}
      }}
    }
  }

  extension (lines: Seq[String]) {
    def findAll(what: String): Int = {
      lines.foldLeft(0) { (matchCount, line) => {
        val parser: scala.util.matching.Regex = what.r
        matchCount + parser.findAllIn(line).size
      }}
    }

    def transpose0: Seq[String] = {
      val transposed: Seq[Seq[Char]] = lines.map(_.toSeq).transpose
      transposed.map(_.mkString)
    }

    def diagonalizeTLBR: Seq[(Position, String)] = {
      val grid: Seq[Seq[Char]] = lines.map(_.toSeq)
      val rows = grid.length
      val cols = grid(0).length

      def collectDiagonal(startRow: Int, startCol: Int): (Position, String) = {
        val builder = new StringBuilder()
        var row = startRow
        var col = startCol

        while (row < rows && col < cols) {
          builder += grid(row)(col)
          row += 1
          col += 1
        }

        ((startRow, startCol), builder.result())
      }

      val diagonals =
        (0 until cols).map(col => collectDiagonal(0, col)) ++
          (1 until rows).map(row => collectDiagonal(row, 0))
      diagonals
    }

    def diagonalizeTRBL: Seq[(Position, String)] = {
      val grid: Seq[Seq[Char]] = lines.map(_.toSeq)
      val rows = grid.length
      val cols = grid(0).length

      def collectDiagonal(startRow: Int, startCol: Int): (Position, String) = {
        val builder = new StringBuilder()
        var row = startRow
        var col = startCol

        while (row < rows && col >= 0) {
          builder += grid(row)(col)
          row += 1
          col -= 1
        }

        ((startRow, startCol), builder.result())
      }

      val diagonals =
        (0 until rows).map(row => collectDiagonal(row, cols - 1)) ++
          (0 until cols - 1).map(col => collectDiagonal(0, col))
      diagonals
    }
  }

  /** @return the solution for part1 */
  def part1(puzzle: Seq[String]): Int = {
    require(puzzle.nonEmpty, "puzzle.nonEmpty")
    logger.debug(s"puzzle: ${puzzle}")

    val what = "XMAS"

    puzzle.findAll(what)
    + puzzle.findAll(what.reverse)
    + puzzle.transpose0.findAll(what)
    + puzzle.transpose0.findAll(what.reverse)
    + puzzle.diagonalizeTLBR.map(_._2).findAll(what)
    + puzzle.diagonalizeTLBR.map(_._2).findAll(what.reverse)
    + puzzle.diagonalizeTRBL.map(_._2).findAll(what)
    + puzzle.diagonalizeTRBL.map(_._2).findAll(what.reverse)
  }

  /** @return the solution for part2 */
  def part2(puzzle: Seq[String]): Int = {
    require(puzzle.nonEmpty, "puzzle.nonEmpty")
    logger.debug(s"puzzle: ${puzzle}")

    val what = "MAS"

    val positionsTLBR =
      puzzle.diagonalizeTLBR.findAllPosTLBR(what)
      ++ puzzle.diagonalizeTLBR.findAllPosTLBR(what.reverse)

    val positionsTRBL =
      puzzle.diagonalizeTRBL.findAllPosTRBL(what)
      ++ puzzle.diagonalizeTRBL.findAllPosTRBL(what.reverse)

    positionsTRBL.intersect(positionsTLBR).size
  }
}
