package aoc2024

class Day11Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day11 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day11 - readFile") {
    val obtained = Day11.readFile("./inputs/Day11.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day11 - part1") {
    val obtained = Day11.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day11 - part2") {
    val obtained = Day11.part2(input)
    assertEquals(obtained, 0)
  }
}
