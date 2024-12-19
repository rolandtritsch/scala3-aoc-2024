package aoc2024

/** Day19 - Linen Layout
  *
  * Seems that (at least for part1) we can cheat a bit and use regular
  * expressions to do the job for us.
  * 
  * The first line of the inpu gives us a list of tokens. We can then
  * pattern match on the given token strings to find the ones that
  * are possible.
  * 
  * Part1:
  * 
  * - Read the input file
  * - Build the regular expression from the tokens
  * - Match the token strings
  * - Count the matches
  */

object Day19 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Towel = String
  type Design = String

  /** @return the set of tokens */ 
  def readFile(filename: String): (Set[Towel], Set[Design]) = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      val lines = source.getLines().toSeq
      val towels = lines.head.split(",").map(_.trim).toSet
      val designs = lines.tail.filter(!_.isEmpty).map(_.trim).toSet
      (towels, designs)
    } finally {
      source.close()
    }
  }

  /** @return the number of possible towels */
  def part1(onsen: (Set[Towel], Set[Design])): Int = {
    require(onsen._1.nonEmpty, "onsen._1.nonEmpty")
    require(onsen._2.nonEmpty, "onsen._2.nonEmpty")
    logger.debug(s"onsen: ${onsen}")

    val (towels, designs) = onsen    
    val parser = raw"(${towels.mkString("|")})+".r
    designs.count(parser.matches)
  }

  /** @return the solution for part2 */
  def part2(onsen: (Set[Towel], Set[Design])): Int = {
    require(onsen._1.nonEmpty, "onsen._1.nonEmpty")
    require(onsen._2.nonEmpty, "onsen._2.nonEmpty")
    logger.debug(s"onsen: ${onsen}")

    val (towels, designs) = onsen
    designs.size
  }
}
