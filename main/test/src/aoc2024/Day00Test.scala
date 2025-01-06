package aoc2024

import aoc2024.Day00._

class Day00Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day00 - dummy".tag(ignore)):
    assert(true)

  test("Day00 - readFile - test"):
    val obtained = readFile("./inputs/Day00Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))

  test("Day00 - readFile"):
    val obtained = readFile("./inputs/Day00.txt")
    assertEquals(obtained, Seq(1, 2, 3))

  test("Day00 - part1 - test"):
    val input = readFile("./inputs/Day00Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)

  test("Day00 - part1"):
    val input = readFile("./inputs/Day00.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1)

  test("Day00 - part2 - test"):
    val input = readFile("./inputs/Day00Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)

  test("Day00 - part2"):
    val input = readFile("./inputs/Day00.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)
