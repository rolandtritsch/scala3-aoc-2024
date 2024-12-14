package aoc2024

/** Day00 - template
  */

object Day00 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Seq[Int] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        val parsed = line.toInt
        logger.debug(s"parsed: ${parsed}")
        parsed
      }
    } finally {
      source.close()
    }
  }

  /** @return the solution for part1 */
  def part1(is: Seq[Int]): Int = {
    require(is.nonEmpty, "is.nonEmpty")
    logger.debug(s"is: ${is}")

    is(0)
  }

  /** @return the solution for part2 */
  def part2(is: Seq[Int]): Int = {
    require(is.nonEmpty, "is.nonEmpty")
    logger.debug(s"is: ${is}")

    is(0)
  }
}
