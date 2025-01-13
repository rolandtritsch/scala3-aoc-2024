package aoc2024

import aoc2024.Day25.*

class Day25Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day25 - dummy".tag(ignore)):
    assert(true)

  test("Day25 - readFile - test"):
    val (keys, locks) = readFile("./inputs/Day25Test.txt")
    assertEquals(keys.size, 3)
    assertEquals(locks.size, 2)

  test("Day25 - readFile"):
    val (keys, locks) = readFile("./inputs/Day25.txt")
    assertEquals(keys.size, 250)
    assertEquals(locks.size, 250)

  test("Day25 - part1 - test"):
    val input = readFile("./inputs/Day25Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 3)

  test("Day25 - part1"):
    val input = readFile("./inputs/Day25.txt")
    val obtained = part1(input)
    assertEquals(obtained, 3269)

  test("Day25 - part2 - test"):
    val input = readFile("./inputs/Day25Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)

  test("Day25 - part2"):
    val input = readFile("./inputs/Day25.txt")
    val obtained = part2(input)
    assertEquals(obtained, 1)

end Day25Test
