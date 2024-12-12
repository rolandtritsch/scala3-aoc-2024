package aoc2024

class Day12Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day12 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day12 - readFile - test") {
    val obtained = Day12.readFile("./inputs/Day12Test.txt")
    assertEquals(obtained.regions.size, 100)
  }

  test("Day12 - readFile") {
    val obtained = Day12.readFile("./inputs/Day12.txt")
    assertEquals(obtained.regions.size, 19600)
  }

  test("Day12 - part1 - test".tag(only)) {
    val input = Day12.readFile("./inputs/Day12Test3.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day12 - part1") {
    val input = Day12.readFile("./inputs/Day12.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 0)
  }

  test("Day12 - part2 - test") {
    val input = Day12.readFile("./inputs/Day12Test.txt")
    val obtained = Day12.part2(input)
    assertEquals(obtained, 0)
  }

  test("Day12 - part2") {
    val input = Day12.readFile("./inputs/Day12.txt")
    val obtained = Day12.part2(input)
    assertEquals(obtained, 0)
  }
}
