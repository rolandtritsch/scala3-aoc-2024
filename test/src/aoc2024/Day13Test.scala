package aoc2024

class Day13Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day13 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day13 - readFile") {
    val obtained = Day13.readFile("./inputs/Day13.txt")
    assertEquals(obtained, Seq(1, 2, 3))
  }

  test("Day13 - part1 - test") {
    val input = Day13.readFile("./inputs/Day13.txt")
    val obtained = Day13.part1(input)
    assertEquals(obtained, 1)
  }

  test("Day13 - part1") {
    val input = Day13.readFile("./inputs/Day13.txt")
    val obtained = Day13.part1(input)
    assertEquals(obtained, 1)
  }

  test("Day13 - part2 - test") {
    val input = Day13.readFile("./inputs/Day13.txt")
    val obtained = Day13.part2(input)
    assertEquals(obtained, 1)
  }

  test("Day13 - part2") {
    val input = Day13.readFile("./inputs/Day13.txt")
    val obtained = Day13.part2(input)
    assertEquals(obtained, 1)
  }
}
