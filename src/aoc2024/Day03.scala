package aoc2024

/** Day03 - Mull It Over
  */

object Day03 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  abstract class Operation
  case class Mul(op1: Int, op2: Int) extends Operation

  private def execute(instructions: Seq[Operation]): Int = {
    instructions.foldLeft(0) { (acc, operation) => operation match {
      case Mul(op1, op2) => acc + (op1 * op2)
      case _ => throw new RuntimeException("Unexpected case")
    }}
  }

  /** @return the solution for part1 */
  def part1(instructions: Seq[Operation]): Int = {
    require(instructions.nonEmpty, "instructions.nonEmpty")
    logger.debug(s"instructions: ${instructions}")

    execute(instructions)
  }

  /** @return the solution for part2 */
  def part2(instructions: Seq[Operation]): Int = {
    require(instructions.nonEmpty, "instructions.nonEmpty")
    logger.debug(s"instructions: ${instructions}")

    val Mul(op1, op2) = instructions(0) : @unchecked
    op1 * op2
  }

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Seq[Operation] = {
    import scala.io.Source
    import scala.util.matching

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line => {
        logger.debug(s"line: ${line}")

        val parseLine: matching.Regex = """mul\(\d{1,3},\d{1,3}\)""".r
        val parsed = parseLine.findAllIn(line).toList
        logger.debug(s"parsed: ${parsed}")

        val parseInstruction: matching.Regex = """(\w+)\s*\((\d+),\s*(\d+)\)""".r
        parsed.map { instruction => instruction match {
          case parseInstruction("mul", op1, op2) => Mul(op1.toInt, op2.toInt)
          case _ => throw new RuntimeException("Unexpected case")
        }}
      }}.flatten
    } finally {
      source.close()
    }
  }
}
