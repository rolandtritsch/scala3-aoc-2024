package aoc2024

import com.typesafe.scalalogging.Logger

/** Day22 - Monkey Market */

object Day22:
  val logger: Logger = Logger(this.getClass.getName)

  type Secret = Long

  /** @return the Set of secret numbers */
  def readFile(filename: String): Set[Secret] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try source.getLines().map(_.toLong).toSet
    finally source.close()
  end readFile

  /** @return the sum nth secret number for every buyer */
  def part1(secrets: Set[Secret]): Long =
    require(secrets.nonEmpty, "secrets.nonEmpty")
    secrets.min

  /** @return the solution for part2 */
  def part2(secrets: Set[Secret]): Long =
    require(secrets.nonEmpty, "secrets.nonEmpty")
    secrets.sum

end Day22
