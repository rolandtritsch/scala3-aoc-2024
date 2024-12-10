package aoc2024

class Day10Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day10 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day10 - readFile") {
    val obtained = Day10.readFile("./inputs/Day10.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day10 - part1") {
    val obtained = Day10.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day10 - part2") {
    val obtained = Day10.part2(input)
    assertEquals(obtained, 0)
  }
}
