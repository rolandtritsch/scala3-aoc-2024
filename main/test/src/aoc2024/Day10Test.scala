package aoc2024

class Day10Test extends munit.ScalaCheckSuite:
  val only   = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  import aoc2024.Day10.*

  test("Day10 - dummy".tag(ignore)):
    assert(true)

  test("Day10 - readFile - test"):
    val obtained = Day10.readFile("./inputs/Day10Test.txt")
    assertEquals(obtained(Position(0, 0)), 8)

  test("Day10 - readFile"):
    val obtained = Day10.readFile("./inputs/Day10.txt")
    assertEquals(obtained(Position(0, 0)), 2)

  test("Day10 - trailHeads - test"):
    val obtained = Day10.readFile("./inputs/Day10Test.txt")
    val expected = Set(
      Position(0, 2),
      Position(0, 4),
      Position(2, 4),
      Position(4, 6),
      Position(5, 2),
      Position(5, 5),
      Position(6, 0),
      Position(6, 6),
      Position(7, 1),
    )
    assertEquals(obtained.trailHeads, expected)

  test("Day10 - part1 - test"):
    val input    = Day10.readFile("./inputs/Day10Test.txt")
    val obtained = Day10.part1(input)
    assertEquals(obtained, 36)

  test("Day10 - part1"):
    val input    = Day10.readFile("./inputs/Day10.txt")
    val obtained = Day10.part1(input)
    assertEquals(obtained, 607)

  test("Day10 - part2 - test"):
    val input    = Day10.readFile("./inputs/Day10Test.txt")
    val obtained = Day10.part2(input)
    assertEquals(obtained, 81)

  test("Day10 - part2"):
    val input    = Day10.readFile("./inputs/Day10.txt")
    val obtained = Day10.part2(input)
    assertEquals(obtained, 1384)
