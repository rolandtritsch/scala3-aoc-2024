package aoc2024

import aoc2024.Day22._

class Day22Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day22 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day22 - readFile - test") {
    val obtained = readFile("./inputs/Day22Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day22 - readFile") {
    val obtained = readFile("./inputs/Day22.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day22 - part1 - test") {
    val input = readFile("./inputs/Day22Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day22 - part1") {
    val input = readFile("./inputs/Day22.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)
  }

  test("Day22 - part2 - test") {
    val input = readFile("./inputs/Day22Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 6)
  }

  test("Day22 - part2") {
    val input = readFile("./inputs/Day22.txt")
    val obtained = part2(input)
    assertEquals(obtained, 6)
  }
}
