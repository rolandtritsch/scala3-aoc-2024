package aoc2024

import aoc2024.Day22.*

class Day22Test extends munit.ScalaCheckSuite:
  val only   = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day22 - dummy".tag(ignore)):
    assert(true)

  test("Day22 - readFile - test"):
    val obtained = readFile("./inputs/Day22Test.txt")
    assertEquals(obtained, Set(1L, 10L, 100L, 2024L))

  test("Day22 - readFile"):
    val obtained = readFile("./inputs/Day22.txt")
    assertEquals(obtained.size, 2244)

  test("Day22 - part1 - test"):
    val input    = readFile("./inputs/Day22Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1L)

  test("Day22 - part1"):
    val input    = readFile("./inputs/Day22.txt")
    val obtained = part1(input)
    assert(obtained > 0L)

  test("Day22 - part2 - test"):
    val input    = readFile("./inputs/Day22Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 2135L)

  test("Day22 - part2"):
    val input    = readFile("./inputs/Day22.txt")
    val obtained = part2(input)
    assertEquals(obtained, 19105311153L)
