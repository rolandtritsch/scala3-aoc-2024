package aoc2024

class Day01Test extends munit.ScalaCheckSuite {
  val input = Seq(
    (3, 4),
    (4, 3),
    (2, 5),
    (1, 3),
    (3, 9),
    (3, 3)
  )

  test("Day01 - part1") {
    val obtained = Day01.part1(input)
    assertEquals(obtained, 11)
  }

  test("Day01 - part2") {
    val obtained = Day01.part2(input)
    assertEquals(obtained, 31)
  }
}
