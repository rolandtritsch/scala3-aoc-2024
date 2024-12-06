package aoc2024

class Day00Test extends munit.ScalaCheckSuite {
  val ignore = new munit.Tag("ignore")

  test("Day00 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day00 - readFile") {
    val obtained = Day00.readFile("./inputs/Day00.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day00 - part1") {
    val obtained = Day00.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day00 - part2") {
    val obtained = Day00.part2(input)
    assertEquals(obtained, 0)
  }
}
