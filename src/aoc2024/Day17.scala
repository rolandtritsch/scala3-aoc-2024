package aoc2024

/** Day17 - Chronospatial Computer
  *
  * This is computer/state machine puzzle.
  * 
  * We need to model the concept of Registers (A, B, C)
  * and the Program (a sequence of Instructions).
  * 
  * Then we need to execute the Program.
  * 
  * We also need a ProgramCounter.
  * 
  * Means a Program has the current Registers and the current
  * ProgramCounter and the set of Instructions.
  */

object Day17 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Registers = Map[Char, Int]

  sealed trait Instruction
  case class ADV(operand: Int) extends Instruction
  case class BXL(operand: Int) extends Instruction
  case class BST(operand: Int) extends Instruction
  case class JNZ(operand: Int) extends Instruction
  case class BXC(operand: Int) extends Instruction  
  case class OUT(operand: Int) extends Instruction
  case class BDV(operand: Int) extends Instruction
  case class CDV(operand: Int) extends Instruction

  object Instruction {
    def create(instruction: Int, operand: Int): Instruction = instruction match {
      case 0 => ADV(operand)
      case 1 => BXL(operand)
      case 2 => BST(operand)
      case 3 => JNZ(operand)
      case 4 => BXC(operand)
      case 5 => OUT(operand)
      case 6 => BDV(operand)
      case 7 => CDV(operand)
      case _ => throw new RuntimeException("Unexpected case")
    } 
  }

  class Program(val counter: Int, registers: Registers, instructions: Seq[Instruction])

  /** @return the registers from the given file */ 
  def readFileRegisters(filename: String): Registers = {
    import scala.io.Source
    import scala.util.matching

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        // Register A: 46337277
        val parser: matching.Regex = """Register (\w): (\d+)""".r
        val parsed = parser.findAllIn(line).matchData.next.subgroups
        assert(parsed.size == 2, s"parsed.size == 2: ${parsed.size}")
        logger.debug(s"parsed: ${parsed}")
        (parsed(0).charAt(0), parsed(1).toInt)
      }.toMap
    } finally {
      source.close()
    }
  }

  /** @return the Instructions from the given file */ 
  def readFileInstructions(filename: String): Seq[Instruction] = {
    import scala.io.Source
    import scala.util.matching

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.flatMap { line =>
        logger.debug(s"line: ${line}")
        // Program: 2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0
        val parser: matching.Regex = """Program: (\d+(?:,\s*\d+)*)""".r
        val parsed = parser.findAllIn(line).matchData.next.subgroups.head.split(",").map(_.toInt).grouped(2).map(pair => (pair(0), pair(1)))
        logger.debug(s"parsed: ${parsed}")
        parsed.map { (instruction, operand) => Instruction.create(instruction, operand) }
      }
    } finally {
      source.close()
    }
  }

  /** @return the list of outputs (as a csv string) */
  def part1(start: (Registers, Seq[Instruction])): Int = {
    require(start._1.nonEmpty, "start._1.nonEmpty")
    require(start._2.nonEmpty, "start._2.nonEmpty")
    logger.debug(s"start: ${start}")

    val (registers, instructions) = start
    val program = new Program(0, registers, instructions)

    program.counter
  }

  def part2(start: (Registers, Seq[Instruction])): Int = {
    require(start._1.nonEmpty, "start._1.nonEmpty")
    require(start._2.nonEmpty, "start._2.nonEmpty")
    logger.debug(s"start: ${start}")

    val (registers, instructions) = start
    val program = new Program(0, registers, instructions)

    program.counter
  }
}
