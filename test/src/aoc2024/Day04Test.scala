package aoc2024

class Day04Test extends munit.ScalaCheckSuite {
  test("Day04 - readFile") {
    val obtained = Day04.readFile("./inputs/Day04.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day04 - part1") {
    val obtained = Day04.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day04 - part2") {
    val obtained = Day04.part2(input)
    assertEquals(obtained, 0)
  }
}
