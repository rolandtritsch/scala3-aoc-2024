package aoc2024

class Day07Test extends munit.ScalaCheckSuite {
  val ignore = new munit.Tag("ignore")

  test("Day07 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day07 - readFile") {
    val obtained = Day07.readFile("./inputs/Day07.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day07 - part1") {
    val obtained = Day07.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day07 - part2") {
    val obtained = Day07.part2(input)
    assertEquals(obtained, 0)
  }
}
