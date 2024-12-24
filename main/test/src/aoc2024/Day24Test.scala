package aoc2024

import aoc2024.Day24._

class Day24Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day24 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day24 - readFile - test") {
    val obtained = readFile("./inputs/Day24Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day24 - readFile") {
    val obtained = readFile("./inputs/Day24.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day24 - part1 - test") {
    val input = readFile("./inputs/Day24Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day24 - part1") {
    val input = readFile("./inputs/Day24.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day24 - part2 - test") {
    val input = readFile("./inputs/Day24Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day24 - part2") {
    val input = readFile("./inputs/Day24.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
