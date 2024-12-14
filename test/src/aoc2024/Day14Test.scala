package aoc2024

class Day14Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day14 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day14 - readFile - test") {
    val obtained = Day14.readFile("./inputs/Day14Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day14 - readFile") {
    val obtained = Day14.readFile("./inputs/Day14.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day14 - part1 - test") {
    val input = Day14.readFile("./inputs/Day14.txt")
    val obtained = Day14.part1(input)
    assertEquals(obtained, 1)
  }

  test("Day14 - part1") {
    val input = Day14.readFile("./inputs/Day14.txt")
    val obtained = Day14.part1(input)
    assertEquals(obtained, 1)
  }

  test("Day14 - part2 - test") {
    val input = Day14.readFile("./inputs/Day14.txt")
    val obtained = Day14.part2(input)
    assertEquals(obtained, 1)
  }

  test("Day14 - part2") {
    val input = Day14.readFile("./inputs/Day14.txt")
    val obtained = Day14.part2(input)
    assertEquals(obtained, 1)
  }
}
