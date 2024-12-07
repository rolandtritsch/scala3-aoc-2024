package aoc2024

class Day07Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day07 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day07 - readFile - test") {
    val obtained = Day07.readFile("./inputs/Day07Test.txt")
    val expected = Day07.Equation(190, List(10, 19))

    assertEquals(obtained(0), expected)
  }

  test("Day07 - readFile") {
    val obtained = Day07.readFile("./inputs/Day07.txt")
    val expected = Day07.Equation(27905293, List(1, 3, 67, 91, 5, 5, 293))

    assertEquals(obtained(0), expected)
  }

  val input = Day07.readFile("./inputs/Day07Test.txt")

  test("Day07 - isValid".tag(only)) {
    assert(input(0).isValid())
    assert(!input(2).isValid())
    assert(!input(4).isValid())
    assert(input(4).isValid(true))
  }

  test("Day07 - part1 - test") {
    val obtained = Day07.part1(input)
    assertEquals(obtained, BigInt(3749))
  }

  test("Day07 - part1") {
    val input = Day07.readFile("./inputs/Day07.txt")
    val obtained = Day07.part1(input)
    assertEquals(obtained, BigInt("28730327770375"))
  }

  test("Day07 - part2 - test") {
    val obtained = Day07.part2(input)
    assertEquals(obtained, BigInt(11387))
  }

  test("Day07 - part2") {
    val input = Day07.readFile("./inputs/Day07.txt")
    val obtained = Day07.part2(input)
    assertEquals(obtained, BigInt("424977609625985"))
  }
}
