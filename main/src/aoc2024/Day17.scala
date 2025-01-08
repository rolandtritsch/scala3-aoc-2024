package aoc2024

/** Day17 - Chronospatial Computer
  *
  * This is computer/state machine puzzle.
  *
  * We need to model the concept of Registers (A, B, C) and the Program (a
  * sequence of Instructions).
  *
  * Then we need to execute the Program.
  *
  * We also need a ProgramCounter.
  *
  * Means a Program has the current Registers and the current ProgramCounter and
  * the set of Instructions.
  */

object Day17:
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    type Operand = Long
    type Registers = Map[Char, Operand]

    sealed trait Instruction:
        def operandLiteral: Int
        def execute(registers: Registers): Registers

        def operand(registers: Registers, literal: Int): Operand = literal match
            case 0 | 1 | 2 | 3 => literal.toLong
            case 4             => registers('A')
            case 5             => registers('B')
            case 6             => registers('C')
            case 7             => ???
            case _             => throw new RuntimeException("Unexpected case")
    end Instruction

    case class ADV(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = registers('A') / (1L << operand(registers))
            registers.updated('A', result)
    end ADV

    case class BXL(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = registers('B') ^ operandLiteral.toLong
            registers.updated('B', result)
    end BXL

    case class BST(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = operand(registers) % 8
            registers.updated('B', result)
    end BST

    case class JNZ(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            if registers('A') == 0 then registers
            else registers.updated('Z', operandLiteral.toLong)
    end JNZ

    case class BXC(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = registers('B') ^ registers('C')
            registers.updated('B', result)
    end BXC

    case class OUT(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = operand(registers) % 8
            registers.updated('Y', result)
    end OUT

    case class BDV(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = registers('A') / (1L << operand(registers))
            registers.updated('B', result)
    end BDV

    case class CDV(operandLiteral: Int) extends Instruction:

        override def operand(
            registers: Registers,
            literal: Int = operandLiteral,
        ): Operand = super.operand(registers, literal)

        override def execute(registers: Registers): Registers =
            val result = registers('A') / (1L << operand(registers))
            registers.updated('C', result)
    end CDV

    object Instruction:
        /** @return
          *   the (new) Instruction for the given (instruction, operand)
          */
        def create(instruction: Int, operandLiteral: Int): Instruction =
            instruction match
                case 0 => ADV(operandLiteral)
                case 1 => BXL(operandLiteral)
                case 2 => BST(operandLiteral)
                case 3 => JNZ(operandLiteral)
                case 4 => BXC(operandLiteral)
                case 5 => OUT(operandLiteral)
                case 6 => BDV(operandLiteral)
                case 7 => CDV(operandLiteral)
                case _ => throw new RuntimeException("Unexpected case")
            end match
        end create
    end Instruction

    /** A Program (State). At the current (program)counter. */
    class Program(
        val counter: Int,
        val registers: Registers,
        val instructions: Seq[Instruction],
        val outputs: List[Operand],
    ):
        /** @return
          *   true if the program is finished (has reached the halted state;
          *   there is no instruction at the current counter)
          */
        def halted: Boolean = counter < 0 || counter >= instructions.size

        /** @return
          *   the next Program (State)
          */
        def next: Program =
            val instruction = instructions(counter)
            val newRegisters = instruction.execute(registers)
            val newCounter =
                if newRegisters.contains('Z') then newRegisters('Z').toInt
                else counter + 1
            val newOutputs =
                if newRegisters.contains('Y') then newRegisters('Y') :: outputs
                else outputs
            // format: off
            new Program(
                newCounter,
                newRegisters.removed('Z').removed('Y'),
                instructions,
                newOutputs,
            )
            // format: on
        end next

        /** @return
          *   the final Program (State) (after running the Program recursively
          *   and it has reached the halted state)
          */
        def run: Program = if halted then this else next.run
    end Program

    /** @return the registers from the given file */
    def readFileRegisters(filename: String): Registers =
        import scala.io.Source
        import scala.util.matching

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try
            val registers = source.getLines().toSeq.map: line =>
                logger.debug(s"line: ${line}")
                // Register A: 46337277
                val parser: matching.Regex = """Register (\w): (\d+)""".r
                val parsed = parser.findAllIn(line).matchData.next.subgroups
                assert(parsed.size == 2, s"parsed.size == 2: ${parsed.size}")
                logger.debug(s"parsed: ${parsed}")
                (parsed(0).charAt(0), parsed(1).toLong)

            registers.toMap.withDefault(_ => Long.MinValue)
        finally source.close()
        end try
    end readFileRegisters

    /** @return the Instructions from the given file */
    def readFileInstructions(filename: String): Seq[Instruction] =
        import scala.io.Source
        import scala.util.matching

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try
            source.getLines().toSeq.flatMap: line =>
                logger.debug(s"line: ${line}")
                // Program: 2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0
                val parser: matching.Regex = """Program: (\d+(?:,\s*\d+)*)""".r
                val parsed = parser.findAllIn(line).matchData.next.subgroups
                    .head.split(",").map(_.toInt).grouped(2)
                    .map(pair => (pair(0), pair(1)))
                logger.debug(s"parsed: ${parsed}")
                parsed.map: (instruction, operandLiteral) =>
                    Instruction.create(instruction, operandLiteral)
        finally source.close()
        end try
    end readFileInstructions

    /** @return the list of outputs (as a csv string) */
    def part1(start: (Registers, Seq[Instruction])): String =
        require(start._1.nonEmpty, "start._1.nonEmpty")
        require(start._2.nonEmpty, "start._2.nonEmpty")
        logger.debug(s"start: ${start}")

        val (registers, instructions) = start
        val program = new Program(0, registers, instructions, List.empty)

        program.run.outputs.reverse.mkString(",")
    end part1

    def part2(start: (Registers, Seq[Instruction])): Int =
        require(start._1.nonEmpty, "start._1.nonEmpty")
        require(start._2.nonEmpty, "start._2.nonEmpty")
        logger.debug(s"start: ${start}")

        val (registers, instructions) = start
        val program = new Program(0, registers, instructions, List.empty)

        program.counter
    end part2
end Day17
