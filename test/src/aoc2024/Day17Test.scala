package aoc2024

import aoc2024.Day17._

class Day17Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day17 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day17 - readFile - test") {
    val obtained = readFile("./inputs/Day17Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day17 - readFile") {
    val obtained = readFile("./inputs/Day17.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day17 - part1 - test") {
    val input = readFile("./inputs/Day17Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day17 - part1") {
    val input = readFile("./inputs/Day17.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day17 - part2 - test") {
    val input = readFile("./inputs/Day17Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day17 - part2") {
    val input = readFile("./inputs/Day17.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
