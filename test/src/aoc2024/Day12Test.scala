package aoc2024

class Day12Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day12 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day12 - readFile - test") {
    val obtained = Day12.readFile("./inputs/Day12Test.txt")
    assertEquals(obtained.plotsByPlant.size, 9)
  }

  test("Day12 - readFile") {
    val obtained = Day12.readFile("./inputs/Day12.txt")
    assertEquals(obtained.plotsByPlant.size, 26)
  }

  test("Day12 - part1 - test") {
    val input = Day12.readFile("./inputs/Day12Test.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 9)
  }

  test("Day12 - part1") {
    val input = Day12.readFile("./inputs/Day12.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 26)
  }

  test("Day12 - part2 - test") {
    val input = Day12.readFile("./inputs/Day12Test.txt")
    val obtained = Day12.part2(input)
    assertEquals(obtained, 9)
  }

  test("Day12 - part2") {
    val input = Day12.readFile("./inputs/Day12.txt")
    val obtained = Day12.part2(input)
    assertEquals(obtained, 26)
  }
}
