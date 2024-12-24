package aoc2024

/** Day24 - Crossed Wires
  * 
  * This is a boolean expression evaluator.
  * 
  * Hhhmmm ... one easy, cheaky way to go about it would be to
  * use Toolbox to evaluate Scala code on the fly.
  * 
  * A more sophisticated, professional approach would be to
  * use a parser combinator and build an AST and then evaluate
  * the AST (e.g. use fastparse).
  * 
  * Let's try both. Let's use Toolbox for part1 and the parser
  * combinator for part2.
  * 
  * Part1:
  * 
  * - Read the input file and build a code file that looks like this ...
  * 
  *   val y01 = false
  *   val y02 = true
  *   val z01 = y01 && y02
  *   z01
  * 
  * - Evaluate the code string
   */

object Day24 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  enum Operation {
    case AND, OR, XOR
  }

  sealed trait LeftHandSide
  case class LeftVariable(name: String) extends LeftHandSide

  sealed trait RightHandSide
  case class RightVariable(name: String) extends RightHandSide
  case class Value(value: Boolean) extends RightHandSide
  case class Expression(left: LeftHandSide, op: Operation, right: RightHandSide) extends RightHandSide

  case class Assignment(left: LeftHandSide, right: RightHandSide)

  /** @return the Assignments for the initial values */ 
  def readFileInitials(filename: String): Set[Assignment] = {
    import scala.io.Source
    
    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        // x00: 1
        val parser = """(\w+): ([0,1])""".r
        val parsed = parser.findAllIn(line).matchData.next.subgroups
        assert(parsed.size == 2, s"parsed.size == 2: ${parsed.size}")
        logger.debug(s"parsed: ${parsed}")
        Assignment(LeftVariable(parsed(0)), Value(parsed(1).toInt == 1))
      }.toSet
    } finally {
      source.close()
    }
  }

  /** @return the Assignments for the statements */ 
  def readFileStatements(filename: String): Set[Assignment] = {
    import scala.io.Source
    
    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        // x00 AND y00 -> z00 
        val parser = """(\w+) (AND|OR|XOR) (\w+) -> (\w+)""".r
        val parsed = parser.findAllIn(line).matchData.next.subgroups
        logger.debug(s"parsed: ${parsed}")
        assert(parsed.size == 4, s"parsed.size == 4: ${parsed.size}")
        Assignment(
          LeftVariable(parsed(3)), 
          Expression(
            LeftVariable(parsed(0)), 
            Operation.valueOf(parsed(1).toUpperCase), 
            RightVariable(parsed(2))
          )
        )
      }.toSet
    } finally {
      source.close()
    }
  }

  /** @return the Int of the resulting bit string */
  def part1 (s: (Set[Assignment], Set[Assignment])): Int = {
    require(s._1.nonEmpty, "s._1.nonEmpty")
    require(s._2.nonEmpty, "s._2.nonEmpty")
    logger.debug(s": ${s}")

    val statements = s._1 ++ s._2

    statements.size
  }

  /** @return the solution for part2 */
  def part2(s: (Set[Assignment], Set[Assignment])): Int = {
    require(s._1.nonEmpty, "s._1.nonEmpty")
    require(s._2.nonEmpty, "s._2.nonEmpty")
    logger.debug(s": ${s}")

    val statements = s._1 ++ s._2

    statements.size
  }
}
