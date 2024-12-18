package aoc2024

import aoc2024.Day18._

class Day18Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day18 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day18 - readFile - test") {
    val obtained = readFile("./inputs/Day18Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day18 - readFile") {
    val obtained = readFile("./inputs/Day18.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day18 - part1 - test") {
    val input = readFile("./inputs/Day18Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day18 - part1") {
    val input = readFile("./inputs/Day18.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day18 - part2 - test") {
    val input = readFile("./inputs/Day18Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day18 - part2") {
    val input = readFile("./inputs/Day18.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
