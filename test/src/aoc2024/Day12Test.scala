package aoc2024

import aoc2024.Day12._

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

  test("Day12 - collectRegion") {
    val garden = Day12.readFile("./inputs/Day12Test2.txt")
    val plots = garden.plotsByPlant('A')
    val (region, remainingPlots) = garden.collectRegion(plots)
    assertEquals(region.area, 4)
    assertEquals(region.perimeter, 10)
    assertEquals(remainingPlots.size, 0)
  }

  test("Day12 - collectRegions") {
    val garden = Day12.readFile("./inputs/Day12Test3.txt")
    val plots = garden.plotsByPlant('X')
    val regions = garden.collectRegions(plots, Set.empty)
    assertEquals(regions.size, 4)
  }

  test("Day12 - part1 - test") {
    val input = Day12.readFile("./inputs/Day12Test.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 1930)
  }

  test("Day12 - part1 - test2") {
    val input = Day12.readFile("./inputs/Day12Test2.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 140)
  }

  test("Day12 - part1 - test3") {
    val input = Day12.readFile("./inputs/Day12Test3.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 772)
  }

  test("Day12 - part1") {
    val input = Day12.readFile("./inputs/Day12.txt")
    val obtained = Day12.part1(input)
    assertEquals(obtained, 1483212)
  }

  test("Day12 - part2 - test") {
    val input = Day12.readFile("./inputs/Day12Test.txt")
    val obtained = Day12.part2(input)
    //assertEquals(obtained, 1206)
    assertEquals(obtained, 1991)
  }

  test("Day12 - part2 - test2") {
    val input = Day12.readFile("./inputs/Day12Test2.txt")
    val obtained = Day12.part2(input)
    //assertEquals(obtained, 80)
    assertEquals(obtained, 79)
  }

  test("Day12 - part2 - test3") {
    val input = Day12.readFile("./inputs/Day12Test3.txt")
    val obtained = Day12.part2(input)
    //assertEquals(obtained, 436)
    assertEquals(obtained, 588)
  }

  test("Day12 - part2") {
    val input = Day12.readFile("./inputs/Day12.txt")
    val obtained = Day12.part2(input)
    assertEquals(obtained, 6567247)
  }
}
