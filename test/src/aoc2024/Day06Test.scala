package aoc2024

class Day06Test extends munit.ScalaCheckSuite {
  test("Day06 - readFile") {
    val obtained = Day06.readFile("./inputs/Day06.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day06 - part1") {
    val obtained = Day06.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day06 - part2") {
    val obtained = Day06.part2(input)
    assertEquals(obtained, 0)
  }
}
