package aoc2024

import aoc2024.Day16._

class Day16Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day16 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day16 - readFile - test") {
    val obtained = readFile("./inputs/Day16Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day16 - readFile") {
    val obtained = readFile("./inputs/Day16.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day16 - part1 - test") {
    val input = readFile("./inputs/Day16Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day16 - part1") {
    val input = readFile("./inputs/Day16.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day16 - part2 - test") {
    val input = readFile("./inputs/Day16Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day16 - part2") {
    val input = readFile("./inputs/Day16.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
