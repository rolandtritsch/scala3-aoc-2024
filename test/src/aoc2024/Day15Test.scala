package aoc2024

class Day15Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day15 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day15 - readFile - test") {
    val obtained = Day15.readFile("./inputs/Day15Test.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day15 - readFile") {
    val obtained = Day15.readFile("./inputs/Day15.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day15 - part1 - test") {
    val input = Day15.readFile("./inputs/Day15Test.txt")
    val obtained = Day15.part1(input)
    assertEquals(obtained, 1)
  }

  test("Day15 - part1") {
    val input = Day15.readFile("./inputs/Day15.txt")
    val obtained = Day15.part1(input)
    assertEquals(obtained, 1)
  }

  test("Day15 - part2 - test") {
    val input = Day15.readFile("./inputs/Day15Test.txt")
    val obtained = Day15.part2(input)
    assertEquals(obtained, 1)
  }

  test("Day15 - part2") {
    val input = Day15.readFile("./inputs/Day15.txt")
    val obtained = Day15.part2(input)
    assertEquals(obtained, 1)
  }
}
