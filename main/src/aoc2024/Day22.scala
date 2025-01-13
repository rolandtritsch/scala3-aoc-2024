package aoc2024

import com.typesafe.scalalogging.Logger

/** Day22 - Monkey Market
  *
  * That looks/sounds simple enough.
  * 
  * Let's go with a LazyList and build the sequence of secrets.
  * We then "just" need to access the nth (2000th) secret and 
  * return the sum of those.
  */

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

  extension (secret: Secret)
    def mix(base: Secret): Secret = secret ^ base
    def prune: Secret = secret % 16777216

  def step1(secret: Secret): Secret = (secret * 64).mix(secret).prune
  def step2(secret: Secret): Secret = (secret / 32).mix(secret).prune
  def step3(secret: Secret): Secret = (secret * 2048).mix(secret).prune

  def next(secret: Secret): Secret = step3(step2(step1(secret)))

  /** @return the sum nth secret number for every buyer */
  def part1(secrets: Set[Secret]): Long =
    require(secrets.nonEmpty, "secrets.nonEmpty")

    val finalSecrets = secrets.map(LazyList.iterate(_)(next)(2000))
    finalSecrets.sum

  /** @return the solution for part2 */
  def part2(secrets: Set[Secret]): Long =
    require(secrets.nonEmpty, "secrets.nonEmpty")
    secrets.sum

end Day22
