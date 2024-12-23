package aoc2024

import aoc2024.Day23._

class Day23Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day23 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day23 - readFile - test") {
    val obtained = readFile("./inputs/Day23Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day23 - readFile") {
    val obtained = readFile("./inputs/Day23.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day23 - part1 - test") {
    val input = readFile("./inputs/Day23Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day23 - part1") {
    val input = readFile("./inputs/Day23.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day23 - part2 - test") {
    val input = readFile("./inputs/Day23Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day23 - part2") {
    val input = readFile("./inputs/Day23.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
