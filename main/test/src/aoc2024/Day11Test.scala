package aoc2024

import aoc2024.Day11.stones

class Day11Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day11 - dummy".tag(ignore)):
    assert(true)

  test("Day11 - readFile - test"):
    val obtained = Day11.readFile("./inputs/Day11Test.txt")
    assertEquals(obtained.head, 125L)

  test("Day11 - readFile"):
    val obtained = Day11.readFile("./inputs/Day11.txt")
    assertEquals(obtained.head, 475449L)

  test("Day11 - apply"):
    val input = "0 1 10 99 999".stones
    val obtained = input.apply(Day11.rules)
    val expected = "1 2024 1 0 9 9 2021976".stones

    assertEquals(obtained, expected)

  test("Day11 - apply"):
    val input = "1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32".stones
    val obtained = input.apply(Day11.rules)
    val expected = "2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2".stones

    assertEquals(obtained, expected)

  test("Day11 - ccount"):
    import aoc2024.Day11.ccount

    val input = "0 1 10 99 999".stones

    assertEquals(input.ccount(Day11.rules, 0), 5L)
    assertEquals(input.ccount(Day11.rules, 1), 7L)

  test("Day11 - ccount"):
    import aoc2024.Day11.ccount

    val input = "125 17".stones

    val obtained = (0 to 6).map(input.ccount(Day11.rules, _)).toList
    val expected = List(2, 3, 4, 5, 9, 13, 22).map(_.toLong)

    assertEquals(obtained, expected)

  test("Day11 - ssize/ccount - test"):
    import aoc2024.Day11.{ssize, ccount}

    val input = Day11.readFile("./inputs/Day11Test.txt")
    val obtained = input.ssize(Day11.rules, 25)
    val expected = input.ccount(Day11.rules, 25)

    assertEquals(obtained, expected)

  test("Day11 - ssize/ccount"):
    import aoc2024.Day11.{ssize, ccount}

    val input = Day11.readFile("./inputs/Day11.txt")
    val obtained = input.ssize(Day11.rules, 25)
    val expected = input.ccount(Day11.rules, 25)

    assertEquals(obtained, expected)

  test("Day11 - part1 - test"):
    val input = Day11.readFile("./inputs/Day11Test.txt")
    val obtained = Day11.part1(input)
    assertEquals(obtained, 55312L)

  test("Day11 - part1"):
    val input = Day11.readFile("./inputs/Day11.txt")
    val obtained = Day11.part1(input)
    assertEquals(obtained, 193269L)

  test("Day11 - part2 - test"):
    val input = Day11.readFile("./inputs/Day11Test.txt")
    val obtained = Day11.part2(input)
    assertEquals(obtained, 65601038650482L)

  test("Day11 - part2"):
    val input = Day11.readFile("./inputs/Day11.txt")
    val obtained = Day11.part2(input)
    assertEquals(obtained, 228449040027793L)

end Day11Test
