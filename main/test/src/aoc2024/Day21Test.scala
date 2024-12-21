package aoc2024

import aoc2024.Day21._

class Day21Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day21 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day21 - readFile - test") {
    val obtained = readFile("./inputs/Day21Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day21 - readFile") {
    val obtained = readFile("./inputs/Day21.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day21 - part1 - test") {
    val input = readFile("./inputs/Day21Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day21 - part1") {
    val input = readFile("./inputs/Day21.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day21 - part2 - test") {
    val input = readFile("./inputs/Day21Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }

  test("Day21 - part2") {
    val input = readFile("./inputs/Day21.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
  }
}
