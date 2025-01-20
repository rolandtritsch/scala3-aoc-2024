package aoc2024

// scalafix:off
import scala.concurrent.duration.Duration

import aoc2024.Day20.*

// scalafix:on
class Day20Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  val slow = new munit.Tag("slow")

  override def munitTimeout: Duration = scala.concurrent.duration.Duration(600, "s")

  test("Day20 - dummy".tag(ignore)):
    assert(true)

  test("Day20 - readFile - test".tag(slow)):
    val grid = readFile("./inputs/Day20Test.txt")
    assertEquals(grid.free.size, 85)
    assertEquals(grid.blocked.size, 140)

  test("Day20 - readFile"):
    val grid = readFile("./inputs/Day20.txt")
    assertEquals(grid.free.size, 9321)
    assertEquals(grid.blocked.size, 10560)

  test("Day20 - path - test"):
    val grid = readFile("./inputs/Day20Test.txt")
    val sp = grid.path
    assertEquals(sp.size, 84)

  test("Day20 - path"):
    val grid = readFile("./inputs/Day20.txt")
    val sp = grid.path
    assertEquals(sp.size, 9320)

  test("Day20 - cheats - test"):
    val grid = readFile("./inputs/Day20Test.txt")
    val cs = grid.cheats(grid.path)
    assertEquals(cs.size, 44)

  test("Day20 - cheats"):
    val grid = readFile("./inputs/Day20.txt")
    val cs = grid.cheats(grid.path)
    assertEquals(cs.size, 6837)

  test("Day20 - part1 - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part1(input, 20)
    assertEquals(obtained, 5)

  test("Day20 - part1".tag(slow)):
    val input = readFile("./inputs/Day20.txt")
    val obtained = part1(input)
    assertEquals(obtained, 1378)

  test("Day20 - part2 - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 140)

  test("Day20 - part2"):
    val input = readFile("./inputs/Day20.txt")
    val obtained = part2(input)
    assertEquals(obtained, 10560)

end Day20Test
