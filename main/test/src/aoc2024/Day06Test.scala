package aoc2024

class Day06Test extends munit.ScalaCheckSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  test("Day06 - readFile - test"):
    val obtained = Day06.readFile("./inputs/Day06Test.txt")
    assertEquals(obtained.guard, Day06.Position(6, 4))

  test("Day06 - readFile"):
    val obtained = Day06.readFile("./inputs/Day06.txt")
    assertEquals(obtained.guard, Day06.Position(53, 89))

  test("Day06 - part1 - test"):
    val testLab = Day06.readFile("./inputs/Day06Test.txt")
    val obtained = Day06.part1(testLab)
    assertEquals(obtained, 41)

  test("Day06 - part1"):
    val lab = Day06.readFile("./inputs/Day06.txt")
    val obtained = Day06.part1(lab)
    assertEquals(obtained, 5531)

  test("Day06 - part2 - test"):
    val testLab = Day06.readFile("./inputs/Day06Test.txt")
    val obtained = Day06.part2(testLab)
    assertEquals(obtained, 6)

  test("Day06 - part2"):
    val lab = Day06.readFile("./inputs/Day06.txt")
    val obtained = Day06.part2(lab)
    assertEquals(obtained, 2165)

end Day06Test
