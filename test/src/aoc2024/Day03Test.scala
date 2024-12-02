package aoc2024

class Day03Test extends munit.ScalaCheckSuite {
  test("Day03 - readFile") {
    val obtained = Day03.readFile("./inputs/Day03.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day03 - part1") {
    val obtained = Day03.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day03 - part2") {
    val obtained = Day03.part2(input)
    assertEquals(obtained, 0)
  }
}
