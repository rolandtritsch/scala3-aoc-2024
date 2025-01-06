package aoc2024

class Day03Test extends munit.ScalaCheckSuite:

  test("Day03 - parser"):
    import aoc2024.Day03.*

    val input                =
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    val instructions         = input.parseLine
    val expectedInstructions =
      List("mul(2,4)", "don't()", "mul(5,5)", "mul(11,8)", "do()", "mul(8,5)")

    assertEquals(instructions, expectedInstructions)

    val operations         = instructions.map(_.parseInstruction)
    val expectedOperations =
      List(Mul(2, 4), Disable(), Mul(5, 5), Mul(11, 8), Enable(), Mul(8, 5))

    assertEquals(operations, expectedOperations)

  test("Day03 - readFile"):
    val obtained = Day03.readFile("./inputs/Day03.txt")
    assertEquals(obtained(0), Day03.Mul(948, 148))

  test("Day03 - part1 - test"):
    import aoc2024.Day03.*

    val input    =
      "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    val obtained = Day03.part1(input.parseLine.map(_.parseInstruction))
    assertEquals(obtained, 161)

  test("Day03 - part1"):
    val input    = Day03.readFile("./inputs/Day03.txt")
    val obtained = Day03.part1(input)
    assertEquals(obtained, 187833789)

  test("Day03 - part2 - test"):
    import aoc2024.Day03.*

    val input    =
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    val obtained = Day03.part2(input.parseLine.map(_.parseInstruction))
    assertEquals(obtained, 48)

  test("Day03 - part2"):
    val input    = Day03.readFile("./inputs/Day03.txt")
    val obtained = Day03.part2(input)
    assertEquals(obtained, 94455185)
