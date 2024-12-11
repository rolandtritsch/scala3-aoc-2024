package aoc2024

/** Day11 - Plutonian Pebbles
  *
  * That sounds easy enough.
  * 
  * Things to think about ...
  *
  * - for every step we need to traverse the entire list
  * - to support the splitting of the stones I was (initially) thinking
  *   to use a double-linked list again, but (because) we traverse (and
  *   modify) the list (and do not need index access to the list elements) 
  *   we probably can (just) use a List
  * - lets assume that the numbers in the list can get big and use a list
  *   of BigInt
  * - to walk the list we can use a loop, a map or a recursion. I will start
  *   with a map and see how it goes
  *
  * part1:
  *
  * - run over the list N times and apply the rules to each element
  * - done
  */

object Day11 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  /** @return parse the given file and return the list of numbers */
  def readFile(filename: String): List[BigInt] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines.next.stones
    } finally {
      source.close()
    }
  }

  type Stone = BigInt
  type Rule = Stone => (List[Stone], Boolean)

  private def ruleZero(stone: Stone): (List[Stone], Boolean) = {
    if(stone == 0) (List(BigInt(1)), true) else (List(BigInt(0)), false)  
  }

  private def ruleEven(stone: Stone): (List[Stone], Boolean) = {
    val stoneString = stone.toString
    if(stoneString.length % 2 == 0) {
      val (first, second) = stoneString.splitAt(stoneString.length / 2)  
      (List(BigInt(first), BigInt(second)), true)
    } else 
      (List(stone), false)  
  }

  private def ruleDefault(stone: Stone): (List[Stone], Boolean) = {
    (List(stone * 2024), true)
  }

  val rules = List(ruleZero, ruleEven, ruleDefault)

  extension (stones: List[BigInt]) {
    def apply(rules: List[Rule]): List[BigInt] = {
      stones.flatMap { stone => 
        rules.foldLeft(List[BigInt](), false) { case ((newStones, done), rule) => 
          if(!done) rule(stone) else (newStones, done)
        }._1
      }
    }

    def applyN(rules: List[Rule], n: Int): List[BigInt] = {
      if (n == 0) stones else stones.apply(rules).applyN(rules, n - 1)
    }
  }

  extension (s: String) {
    def stones: List[BigInt] = s.split(" ").toList.map(BigInt(_))
  }

  /** @return the number of stones on the list */
  def part1(stones: List[BigInt]): BigInt = {
    require(stones.nonEmpty, "stones.nonEmpty")
    logger.debug(s"stones: ${stones}")

    stones.applyN(rules, 25).size
  }

  /** @return the solution for part2 */
  def part2(stones: List[BigInt]): BigInt = {
    require(stones.nonEmpty, "stones.nonEmpty")
    logger.debug(s"stones: ${stones}")

    stones(0)
  }
}
