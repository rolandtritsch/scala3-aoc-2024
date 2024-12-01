package aoc2024

class Day01Test extends munit.ScalaCheckSuite {
  test("Day01 - part1") {
    val obtained = Day01.part1(List(), List())
    assertEquals(obtained, 0)
  }
}
