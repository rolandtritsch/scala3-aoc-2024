package aoc2024

/** Day11 - Plutonian Pebbles
  *
  * That sounds easy enough.
  *
  * Things to think about ...
  *
  *   - for every step we need to traverse the entire list
  *   - to support the splitting of the stones I was (initially) thinking to use
  *     a double-linked list again, but (because) we traverse (and modify) the
  *     list (and do not need index access to the list elements) we probably can
  *     (just) use a List
  *   - lets assume that the numbers in the list can get big and use a (at
  *     least) a Long (maybe even a BigInt)
  *   - to walk the list we can use a loop, a map or a recursion. I will start
  *     with a map and see how it goes
  *
  * part1:
  *
  *   - run over the list N times and apply the rules to each element
  *   - done
  *
  * part2:
  *
  * LOL ... aborted running the part1 solution 75 times after googleplex seconds
  * ...
  *
  *   - let's try a couple of things ...
  *   - let's get rid of building/using the (at the end very long) list. Instead
  *     we can (just) build up the number of elements by going down into the
  *     recursions. For instance ...
  *     - if you have a stone and in N recursions there are no splits than the
  *       number of elements coming from that one stone is 1
  *     - if there is one split it is 2, with two splits it is 4, ...
  *   - means we do not need to build the list to count the elements
  *   - let's also start to cache results, means if we process a stone on level
  *     n somewhere in the tree, than we do not need to process it again. We can
  *     just use the result we already calculated
  *   - last but not least (and I am not sure, if this is going to have a big
  *     impact), we can use parallel collections to count on all cores in
  *     parallel
  */

object Day11:
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    /** @return parse the given file and return the list of numbers */
    def readFile(filename: String): List[Stone] =
        import scala.io.Source

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try source.getLines.next.stones
        finally source.close()
    end readFile

    type Stone = Long
    type RuleResult = (Stone, Option[Stone], Boolean)
    type Rule = Stone => RuleResult

    private def ruleZero(stone: Stone): RuleResult =
        if stone == 0 then (1, None, true) else (0, None, false)

    private def ruleEven(stone: Stone): RuleResult =
        val stoneString = stone.toString
        if stoneString.length % 2 == 0 then
            val (first, second) = stoneString.splitAt(stoneString.length / 2)
            (first.toLong, Some(second.toLong), true)
        else (stone, None, false)
    end ruleEven

    private def ruleDefault(stone: Stone): RuleResult =
        (stone * 2024, None, true)

    // Not private to support testing
    val rules = List(ruleZero, ruleEven, ruleDefault)

    extension (stones: List[Stone])
        /** @return the list of stones after the rules where applied */
        def apply(rules: List[Rule]): List[Stone] =
            stones.flatMap: stone =>
                val (stones, _) = rules.foldLeft(List[Stone](), false): (ss, rule) =>
                    val (newStones, done) = ss
                    if done then (newStones, done)
                    else
                        rule(stone) match
                            case (s, None, done) => (List(s), done)
                            case (s0, Some(s1), done) =>
                                (List(s0, s1), done)
                            case _ =>
                                throw new RuntimeException("Unexpected case")
                        end match
                    end if

                stones
        end apply

        /** @return
          *   the list of stones after the rules have been applied (recursively)
          */
        def applyN(rules: List[Rule], n: Long): List[Stone] =
            if n > 0 then stones.apply(rules).applyN(rules, n - 1) else stones

        /** @return the sum of all (parallel) list sizes */
        def ssize(rules: List[Rule], n: Long): Long =
            import scala.collection.parallel.CollectionConverters.*

            val levelOneStones = stones.apply(rules)
            val levelNStonesSizes = levelOneStones.par.map: stone =>
                List(stone).applyN(rules, n - 1).size

            levelNStonesSizes.sum
        end ssize

        /** @return the sum of all parallel counts */
        def ccount(rules: List[Rule], n: Long): Long =
            import scala.collection.parallel.CollectionConverters.*

            stones.par.map(_.countN(rules, n)).sum
    end extension

    // Yeah ... this will not work without a cache
    private val cache = scala.collection.mutable.Map[(Stone, Long), Long]()

    extension (stone: Stone)

        /** @return the new/next stone(s) after applying the rules */
        def apply(rules: List[Rule]): (Stone, Option[Stone]) =
            val (s0, s1, _) = rules
                .foldLeft(0L, Option(0L), false):(applied, rule) =>
                    val (_, _, done) = applied
                    if done then applied else rule(stone)

            (s0, s1)
        end apply

        /** @return the count of stones for level N */
        def countN(rules: List[Rule], n: Long): Long =
            if n > 0 then
                cache.get(stone, n) match
                    case Some(i) => i
                    case None =>
                        val i = stone.apply(rules) match
                            case (s, None) => s.countN(rules, n - 1)
                            case (s0, Some(s1)) => s0.countN(rules, n - 1) +
                                    s1.countN(rules, n - 1)
                            case _ =>
                                throw new RuntimeException("Unexpected case")
                        cache.put((stone, n), i)
                        i
                end match
            else 1
        end countN
    end extension

    extension (s: String)
        def stones: List[Stone] = s.split(" ").toList.map(_.toLong)

    /** @return the number of stones in the list */
    def part1(stones: List[Stone]): Long =
        require(stones.nonEmpty, "stones.nonEmpty")
        logger.debug(s"stones: ${stones}")

        stones.ssize(rules, 25)
    end part1

    /** @return the number of stones in the (very large) list */
    def part2(stones: List[Stone]): Long =
        require(stones.nonEmpty, "stones.nonEmpty")
        logger.debug(s"stones: ${stones}")

        stones.ccount(rules, 75)
    end part2
end Day11
