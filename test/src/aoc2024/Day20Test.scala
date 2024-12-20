package aoc2024

import aoc2024.Day20._

class Day20Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day20 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day20 - readFile - test") {
    val obtained = readFile("./inputs/Day20Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day20 - readFile") {
    val obtained = readFile("./inputs/Day20.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day20 - part1 - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day20 - part1") {
    val input = readFile("./inputs/Day20.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day20 - part2 - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day20 - part2") {
    val input = readFile("./inputs/Day20.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
