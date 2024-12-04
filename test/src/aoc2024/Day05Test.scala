package aoc2024

class Day05Test extends munit.ScalaCheckSuite {
  test("Day05 - readFile") {
    val obtained = Day05.readFile("./inputs/Day05.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  val input = Seq(0)

  test("Day05 - part1") {
    val obtained = Day05.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day05 - part2") {
    val obtained = Day05.part2(input)
    assertEquals(obtained, 0)
  }
}
