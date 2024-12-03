package aoc2024

class Day03Test extends munit.ScalaCheckSuite {
  import aoc2024.Day03.{Mul, Disable, Enable}

  test("Day03 - readFile") {
    val obtained = Day03.readFile("./inputs/Day03.txt")
    assertEquals(obtained(0), Mul(948,148))
  }

  test("Day03 - part1") {
    val input = Seq(Mul(2, 4), Mul(5, 5), Mul(11, 8), Mul(8, 5))

    val obtained = Day03.part1(input)
    assertEquals(obtained, 161)
  }

  test("Day03 - part2") {
    val input = Seq(Mul(2, 4), Disable(), Mul(5, 5), Mul(11, 8), Enable(), Mul(8, 5))

    val obtained = Day03.part2(input)
    assertEquals(obtained, 48)
  }
}
