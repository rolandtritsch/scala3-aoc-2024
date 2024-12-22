package aoc2024

/** Day21 - Keypad Conundrum
  */

object Day21 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Code = String

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Set[Code] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try {
      source.getLines().toSet
    } finally {
      source.close()
    }
  }

  /** @return the sum of the complexity scores */
  def part1(codes: Set[Code]): Int = {
    require(codes.nonEmpty, "codes.nonEmpty")
    logger.debug(s"codes: ${codes}")

    codes.map(_.size).sum
  }

  /** @return the solution for part2 */
  def part2(codes: Set[Code]): Int = {
    require(codes.nonEmpty, "codes.nonEmpty")
    logger.debug(s"codes: ${codes}")

    codes.map(_.size).sum
  }
}
