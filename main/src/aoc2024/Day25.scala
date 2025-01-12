package aoc2024

import com.typesafe.scalalogging.Logger

/** Day25 - Advent of Code 2024 */

object Day25:
  val logger: Logger = Logger(this.getClass.getName)

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Seq[Int] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try source.getLines().toSeq.map: line =>
        logger.debug(s"line: ${line}")
        val parsed = line.toInt
        logger.debug(s"parsed: ${parsed}")
        parsed
    finally source.close()
    end try
  end readFile

  /** @return the solution for part1 */
  def part1(is: Seq[Int]): Int =
    require(is.nonEmpty, "is.nonEmpty")
    logger.debug(s"is: ${is}")
    1

  /** @return the solution for part2 */
  def part2(is: Seq[Int]): Int =
    require(is.nonEmpty, "is.nonEmpty")
    logger.debug(s"is: ${is}")
    1
  end part2
end Day25
