package aoc2024

/** Day07 - Bridge Repair
  *
  * part1:
  *
  * Hhhmmm ... I think this can be solved with a tree data-structure
  * and a tree-walking algorithm.
  *
  * - the root node has the initial value
  * - you then have one child for '+' and one for '/'
  * - and so on
  * - you then try to find a leaf node that equals the number you are
  *   looking for
  * - this will also support solving more problems for part2
  *
  * Hhhmmm ... that might be too much for part1 and a premature
  * optimization.
  *
  * It is probably just easier to go with a recursion.
  *
  * - take a given value, take the next one from the list and recurse
  *   twice. Once with '+' and once with '*'
  * - if one comes back true, we are good
  *
  * part2:
  *
  * Boy ... am I happy that I did not do the premature optimization.
  * This worked like a charm.
  *
  * Now I only need to add the extra case of ||.
  */

object Day07:
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  case class Equation(val result: BigInt, val numbers: List[BigInt]):
    private def check(current: BigInt, result: BigInt, numbers: List[BigInt], checkConcat: Boolean): Boolean = numbers match
      case Nil => if(current == result) true else false
      case number :: remainingNumbers => {
        logger.debug(s"current: ${current}, result: ${result}, numbers: ${numbers}, checkConcat: ${checkConcat}")

        check(current + number, result, remainingNumbers, checkConcat)
        || check(current * number, result, remainingNumbers, checkConcat)
        || (check(BigInt(current.toString + number.toString), result, remainingNumbers, checkConcat) && checkConcat)
      }

    /** @return true, if the equation is valid */
    def isValid(checkConcat: Boolean = false): Boolean =
      check(numbers.head, result, numbers.tail, checkConcat)

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): List[Equation] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      source.getLines().toList.map { line =>
        logger.debug(s"line: ${line}")

        val parsed = line.split(":")
        logger.debug(s"parsed: ${parsed}")

        val result = BigInt(parsed(0))
        val numbers = parsed(1).trim.split(" ").map(BigInt(_)).toList

        Equation(result, numbers)
      }
    finally
      source.close()

  /** @return the sum of valid equation results */
  def part1(equations: List[Equation]): BigInt =
    require(equations.nonEmpty, "equations.nonEmpty")
    logger.debug(s"equations: ${equations}")

    val validEquations = equations.filter(_.isValid())
    validEquations.map(_.result).sum

  /** @return the sum of valid equation results (with concat) */
  def part2(equations: List[Equation]): BigInt =
    require(equations.nonEmpty, "equations.nonEmpty")
    logger.debug(s"equations: ${equations}")

    val validEquations = equations.filter(_.isValid(true))
    validEquations.map(_.result).sum
