package aoc2024

class Day02Test extends munit.ScalaCheckSuite {
  test("Day02 - readFile") {
    val obtained = Day02.readFile("./inputs/Day02.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day02 - part1") {
    val obtained = Day02.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day02 - part2") {
    val obtained = Day02.part2(input)
    assertEquals(obtained, 0)
  }
}
