package aoc2024

class Day09Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day09 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day09 - readFile") {
    val obtained = Day09.readFile("./inputs/Day09.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day09 - part1") {
    val obtained = Day09.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day09 - part2") {
    val obtained = Day09.part2(input)
    assertEquals(obtained, 0)
  }
}
