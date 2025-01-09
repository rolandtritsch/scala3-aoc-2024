package aoc2024

import com.typesafe.scalalogging.Logger

/** Day03 - Mull It Over
  *
  * This is a computer simulation puzzle.
  *
  * part1:
  *
  *   - we just have one operation (Mul)
  *   - execute the instructions
  *   - return the accumulator
  *
  * part2:
  *
  *   - we have three operations (Mul, Disable, Enable)
  *   - execute the instructions
  *   - return the accumulator
  */

object Day03:
  val logger: Logger = Logger(this.getClass.getName)

  abstract class Operation
  case class Mul(op1: Int, op2: Int) extends Operation
  case class Disable() extends Operation
  case class Enable() extends Operation

  private def execute(instructions: Seq[Operation], checking: Boolean): Int =
    val (result, _) = instructions.foldLeft(0, true): (accumulator, operation) =>
      val (acc, enabled) = accumulator
      operation match
        case Mul(op1, op2) =>
          if enabled || !checking then (acc + (op1 * op2), true) else (acc, false)
        case Disable() => (acc, false)
        case Enable()  => (acc, true)
      end match
    result
  end execute

  /** @return the solution for part1 */
  def part1(instructions: Seq[Operation]): Int =
    require(instructions.nonEmpty, "instructions.nonEmpty")
    logger.debug(s"instructions: ${instructions}")

    execute(instructions, false)
  end part1

  /** @return the solution for part2 */
  def part2(instructions: Seq[Operation]): Int =
    require(instructions.nonEmpty, "instructions.nonEmpty")
    logger.debug(s"instructions: ${instructions}")

    execute(instructions, true)
  end part2

  object Parser:
    import scala.util.matching

    val line: matching.Regex = """mul\(\d{1,3},\d{1,3}\)|do\(\)|don\'t\(\)""".r
    val instruction: matching.Regex = """(mul|do|don\'t)\s*\(((\d+),\s*(\d+))?\)""".r
  end Parser

  extension (toBeParsed: String)
    def parseLine: List[String] = Parser.line.findAllIn(toBeParsed).toList

    def parseInstruction: Operation = toBeParsed match
      case Parser.instruction("mul", _, op1, op2) => Mul(op1.toInt, op2.toInt)
      case Parser.instruction("don't", _, _, _)   => Disable()
      case Parser.instruction("do", _, _, _)      => Enable()
      case _                                      => throw new RuntimeException("Unexpected case")

  end extension

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Seq[Operation] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"${filename}")

    val source = Source.fromResource(filename)
    try source.getLines().toSeq.flatMap: line =>
        logger.debug(s"line: ${line}")

        line.parseLine.map(_.parseInstruction)
    finally source.close()
    end try
  end readFile

end Day03
