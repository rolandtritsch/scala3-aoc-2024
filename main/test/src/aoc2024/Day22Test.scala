package aoc2024

import aoc2024.Day22.*

class Day22Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day22 - dummy".tag(ignore)):
    assert(true)

  test("Day22 - readFile - test"):
    val obtained = readFile("./inputs/Day22Test.txt")
    assertEquals(obtained, Set(1L, 10L, 100L, 2024L))

  test("Day22 - readFile"):
    val obtained = readFile("./inputs/Day22.txt")
    assertEquals(obtained.size, 2244)

  test("Day22 - mix"):
    assertEquals(42L.mix(15L), 37L)

  test("Day22 - prune"):
    assertEquals(100000000L.prune, 16113920L)

  test("Day22 - next"):
    assertEquals(next(123L), 15887950L)

  test("Day22 - secrets(10)"):
    val secrets = LazyList.iterate(123L)(next)
    assertEquals(secrets(10), 5908254L)

  test("Day22 - secrets(2000)"):
    assertEquals(LazyList.iterate(123L)(next)(10), 5908254L)
    assertEquals(LazyList.iterate(1L)(next)(2000), 8685429L)
    assertEquals(LazyList.iterate(10L)(next)(2000), 4700978L)
    assertEquals(LazyList.iterate(100L)(next)(2000), 15273692L)
    assertEquals(LazyList.iterate(2024L)(next)(2000), 8667524L)

  test("Day22 - part1 - test"):
    val input = readFile("./inputs/Day22Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 37327623L)

  test("Day22 - part1"):
    val input = readFile("./inputs/Day22.txt")
    val obtained = part1(input)
    assertEquals(obtained, 18525593556L)

  test("Day22 - part2 - test"):
    val input = readFile("./inputs/Day22Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 2135L)

  test("Day22 - part2"):
    val input = readFile("./inputs/Day22.txt")
    val obtained = part2(input)
    assertEquals(obtained, 19105311153L)

end Day22Test
