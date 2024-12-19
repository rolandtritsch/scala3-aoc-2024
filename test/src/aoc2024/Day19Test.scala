package aoc2024

import aoc2024.Day19._

class Day19Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day19 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day19 - readFile - test") {
    val obtained = readFile("./inputs/Day19Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day19 - readFile") {
    val obtained = readFile("./inputs/Day19.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day19 - part1 - test") {
    val input = readFile("./inputs/Day19Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day19 - part1") {
    val input = readFile("./inputs/Day19.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day19 - part2 - test") {
    val input = readFile("./inputs/Day19Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day19 - part2") {
    val input = readFile("./inputs/Day19.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
