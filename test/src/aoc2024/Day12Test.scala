package aoc2024

class Day12Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day12 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day12 - readFile") {
    val obtained = Day12.readFile("./inputs/Day12.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day12 - part1") {
    val obtained = Day12.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day12 - part2") {
    val obtained = Day12.part2(input)
    assertEquals(obtained, 0)
  }
}
