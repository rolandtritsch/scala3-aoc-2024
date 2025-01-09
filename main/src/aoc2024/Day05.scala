package aoc2024

import com.typesafe.scalalogging.Logger

/** Day05 - Print Queue
  *
  * part1:
  *
  *   - read all the rules (as (before, after) pairs)
  *   - read all the updates (as a Seq of page numbers)
  *   - note: The trick is to realize that you can simplify the rule checking by converting the
  *     updates into a Seq of (before, after) pairs (for every page in the udpate
  *   - check if all update pairs are in the Set of rule pairs
  *   - if so the update is valid
  *   - find the middle pages of all valid updates and sum them up
  *
  * part2:
  *
  *   - look for the invalid updates
  *   - find the ones that can be fixed (and fix them)
  *   - find the middle pages of the (now valid/fixed) updates and sum them up
  */

object Day05:
  val logger: Logger = Logger(this.getClass.getName)

  type Page = Int
  type Rule = (Page, Page)
  type Fact = (Page, Page)
  type Update = List[Page]

  /** @return a Set of Rules */
  def readFileRules(filename: String): Set[Rule] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val rules = source.getLines().map: line =>
        logger.debug(s"line: ${line}")

        val parsed = line.split("\\|").map(_.toInt).toList
        logger.debug(s"parsed: ${parsed}")
        (parsed(0), parsed(1))
      rules.toSet
    finally source.close()
    end try
  end readFileRules

  /** @return a Seq of updates */
  def readFileUpdates(filename: String): Set[Update] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val updates = source.getLines().map: line =>
        logger.debug(s"line: ${line}")

        val parsed = line.split(",").map(_.toInt).toList
        logger.debug(s"parsed: ${parsed}")
        parsed
      updates.toSet
    finally source.close()
    end try
  end readFileUpdates

  extension (update: Update)

    /** @return the Set of facts to check against the rules */
    def facts: Set[Fact] =
      def factBuilder(update: Update, facts: Set[Fact]): Set[Fact] = update match
        case Nil           => facts
        case page :: pages => factBuilder(pages, factBuilder0(page, pages, facts))

      def factBuilder0(thizPage: Int, thizPages: Update, facts: Set[Fact]): Set[Fact] =
        thizPages match
          case Nil           => facts
          case page :: pages => factBuilder0(thizPage, pages, facts).incl((thizPage, page))

      factBuilder(update, Set())
    end facts

    /** @return true, if update is valid for the given set of rules */
    def isValid(rules: Set[Rule]): Boolean = update.facts.subsetOf(rules)

    /** @return invalid facts */
    def invalid(rules: Set[Rule]): Set[Fact] = update.facts.filterNot(rules.contains(_))

    /** @return swap the two elements */
    def swap(fact: Fact): Update =
      val (before, after) = fact
      update.map: p =>
        if p == before then after else if p == after then before else p
    end swap

    /** @return the fixed update */
    def fix(rules: Set[Rule]): Update =
      def fix0(invalid: List[Fact]): Update = invalid match
        case Nil => throw new RuntimeException("No fix found")
        case fact :: rest =>
          val swapped = update.swap(fact)
          logger.info(s"update: ${update}, fact: ${fact}, swapped: ${swapped}")
          if swapped.isValid(rules) then swapped else fix0(rest)

      def fix1(invalid: List[Fact], update: Update): Update = invalid match
        case Nil          => update
        case fact :: rest => fix1(rest, update.swap(fact))

      // fix0(update.invalid(rules).toList)
      fix1(update.invalid(rules).toList.sorted.reverse, update)
    end fix

  end extension

  /** @return the sum of middle page numbers from the valid updates */
  def part1(rules: Set[Rule], updates: Set[Update]): Int =
    require(rules.nonEmpty, "rules.nonEmpty")
    require(updates.nonEmpty, "updates.nonEmpty")
    logger.debug(s"rules: ${rules}, updates: ${updates}")

    val validUpdates = updates.filter(_.isValid(rules))
    val middlePages = validUpdates.toList.map: update =>
      update(update.size / 2)
    middlePages.sum
  end part1

  /** @return the sum of middle pages of the fixed invalid updates */
  def part2(rules: Set[Rule], updates: Set[Update]): Int =
    require(rules.nonEmpty, "rules.nonEmpty")
    require(updates.nonEmpty, "updates.nonEmpty")
    logger.debug(s"rules: ${rules}, updates: ${updates}")

    val inValidUpdates = updates.filterNot(_.isValid(rules))
    val validUpdates = inValidUpdates.map(_.fix(rules))
    val middlePages = validUpdates.toList.map: update =>
      update(update.size / 2)
    middlePages.sum
  end part2

end Day05
