package aoc2024

/** Day24 - Crossed Wires
  *
  * This is a boolean expression evaluator.
  *
  * Hhhmmm ... one easy, cheaky way to go about it would be to use Toolbox to
  * evaluate Scala code on the fly.
  *
  * A more sophisticated, professional approach would be to use a parser
  * combinator and build an AST and then evaluate the AST (e.g. use fastparse).
  *
  * Let's try both. Let's use runtime evaluation for part1 and the parser
  * combinator for part2.
  *
  * Note: To make the runtime evaluation work we have to downgrade Scala to
  * 3.4.3 (com.eed3si9n.eval requirement).
  *
  * Note: "val" will obviously not work, because of the forward references. Then
  * I tried "lazy val", but that will fail with "Platform restriction: a
  * parameter list's length cannot exceed 254." What obviously works is "def".
  *
  * Part1:
  *
  *   - Read the input file and build a code file that looks like this ...
  * ```
  * def y01 = false 
  * def y02 = true 
  * def z00 = y01 && y02 
  * def z01 = y01 || y02
  * List(z01, z00).map(if(_) 1 else 0).mkString
  * ```
  *   - Note: z00 is the lowest bit
  *   - Evaluate the code string
  */

object Day24:
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    enum Operation:
        case AND, OR, XOR

    extension (op: Operation)
        def str: String = op match
            case Operation.AND => "&&"
            case Operation.OR  => "||"
            case Operation.XOR => "^"

    sealed trait LeftHandSide
    case class LeftVariable(name: String) extends LeftHandSide:
        override def toString(): String = s"${name}"

    sealed trait RightHandSide
    case class RightVariable(name: String) extends RightHandSide:
        override def toString(): String = s"${name}"
    case class Value(value: Boolean) extends RightHandSide:
        override def toString(): String = s"${value}"

    case class Expression(
        left: LeftHandSide,
        op: Operation,
        right: RightHandSide,
    ) extends RightHandSide:
        override def toString(): String = s"${left} ${op.str} ${right}"
    end Expression

    case class Assignment(left: LeftHandSide, right: RightHandSide):
        override def toString(): String = s"def ${left} = ${right}"

    /** @return the Assignments for the initial values */
    def readFileInitials(filename: String): Set[Assignment] =
        import scala.io.Source

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try
            val assignments = source.getLines().toSeq.map: line =>
                logger.debug(s"line: ${line}")
                // x00: 1
                val parser = """(\w+): ([0,1])""".r
                val parsed = parser.findAllIn(line).matchData.next.subgroups
                assert(parsed.size == 2, s"parsed.size == 2: ${parsed.size}")
                logger.debug(s"parsed: ${parsed}")
                Assignment(LeftVariable(parsed(0)), Value(parsed(1).toInt == 1))
            assignments.toSet
        finally source.close()
        end try
    end readFileInitials

    /** @return the Assignments for the statements */
    def readFileStatements(filename: String): Set[Assignment] =
        import scala.io.Source

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try 
            val assignments = source.getLines().toSeq.map: line =>
                logger.debug(s"line: ${line}")
                // x00 AND y00 -> z00
                val parser = """(\w+) (AND|OR|XOR) (\w+) -> (\w+)""".r
                val parsed = parser.findAllIn(line).matchData.next.subgroups
                logger.debug(s"parsed: ${parsed}")
                assert(parsed.size == 4, s"parsed.size == 4: ${parsed.size}")
                // format: off
                Assignment(
                    LeftVariable(parsed(3)),
                    Expression(
                        LeftVariable(parsed(0)),
                        Operation.valueOf(parsed(1)),
                        RightVariable(parsed(2)),
                    ),
                )
                // format: on
            assignments.toSet
        finally source.close()
        end try
    end readFileStatements

    /** @return the evaluation of the code */
    def evaluate(lines: String): String =
        com.eed3si9n.eval.Eval()
            .evalInfer(lines)
            .getValue(this.getClass.getClassLoader)
            .toString

    /** @return the code snippet that needs to be evaluated */
    def generate(statements: Set[Assignment]): String =
        val vars = statements.map:
            case Assignment(LeftVariable(name), _) => name
        .filter(_.startsWith("z")).toList.sorted.reverse.mkString(",")
        val convertToBitString = s"List(${vars}).map(if(_) 1 else 0).mkString"

        statements.map(_.toString).toList.sorted.mkString("\n") + "\n" +
            convertToBitString
    end generate

    /** @return the Int of the resulting bit string */
    def part1(s: (Set[Assignment], Set[Assignment])): BigInt =
        require(s._1.nonEmpty, "s._1.nonEmpty")
        require(s._2.nonEmpty, "s._2.nonEmpty")
        logger.debug(s": ${s}")

        val statements = s._1 ++ s._2

        val bits = evaluate(generate(statements))
        BigInt(bits, 2)
    end part1

    /** @return the solution for part2 */
    def part2(s: (Set[Assignment], Set[Assignment])): Int =
        require(s._1.nonEmpty, "s._1.nonEmpty")
        require(s._2.nonEmpty, "s._2.nonEmpty")
        logger.debug(s": ${s}")

        val statements = s._1 ++ s._2

        statements.size
    end part2
end Day24
