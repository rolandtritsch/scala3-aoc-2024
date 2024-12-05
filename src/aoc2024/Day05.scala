package aoc2024

/** Day05 - Print Queue
  *
  * part1:
  *
  *   - read all the rules (as (before, after) pairs)
  *   - read all the updates (as a Seq of page numbers)
  *   - note: The trick is to realize that you can simplify the rule checking by
  *     converting the updates into a Seq of (before, after) pairs (for every
  *     page in the udpate
  *   - check if all update pairs are in the Set of rule pairs
  *   - if so the update is valid
  *   - find the middle pages of all valid updates and sum them up
  *
  * Done.
  */

object Day05 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Page = Int
  type Rule = (Page, Page)
  type Fact = (Page, Page)
  type Update = List[Page]

  /** @return a Set of rules */
  def readFileRules(filename: String): Set[Rule] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source
        .getLines()
        .map { line =>
          logger.debug(s"line: ${line}")

          val parsed = line.split("\\|").map(_.toInt).toList
          logger.debug(s"parsed: ${parsed}")
          (parsed(0), parsed(1))
        }.toSet
    } finally {
      source.close()
    }
  }

  /** @return a Seq of updates */
  def readFileUpdates(filename: String): Set[Update] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source
        .getLines()
        .map { line =>
          logger.debug(s"line: ${line}")

          val parsed = line.split(",").map(_.toInt).toList
          logger.debug(s"parsed: ${parsed}")
          parsed
        }.toSet
    } finally {
      source.close()
    }
  }

  extension (update: Update) {
    /** @return the Set of facts to check against the rules */
    def facts: Set[Fact] = {
      def factBuilder(update: Update, facts: Set[Fact]): Set[Fact] = update match {
        case Nil => facts
        case page :: pages =>
          factBuilder(pages, factBuilder0(page, pages, facts))
      }

      def factBuilder0(thizPage: Int, thizPages: Update, facts: Set[Fact]): Set[Fact] = thizPages match {
        case Nil => facts
        case page :: pages =>
          factBuilder0(thizPage, pages, facts).incl((thizPage, page))
      }

      factBuilder(update, Set())
    }

    /** @return true, if update is valid for the given set of rules */
    def isValid(rules: Set[Rule]): Boolean = update.facts.subsetOf(rules)
  }

  /** @return the sum of middle page numbers from the valid updates  */
  def part1(rules: Set[Rule], updates: Set[Update]): Int = {
    require(rules.nonEmpty, "rules.nonEmpty")
    require(updates.nonEmpty, "updates.nonEmpty")
    logger.debug(s"rules: ${rules}, updates: ${updates}")

    val validUpdates = updates.filter(_.isValid(rules))
    val middlePages = validUpdates.map { update => {
      update(update.size/2)
    }}
    middlePages.sum
  }

  /** @return the solution for part2 */
  def part2(is: Seq[Int]): Int = {
    require(is.nonEmpty, "is.nonEmpty")
    logger.debug(s"is: ${is}")

    is(0)
  }
}
