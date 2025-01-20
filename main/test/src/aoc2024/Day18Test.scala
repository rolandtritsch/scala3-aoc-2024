package aoc2024

// scalafix:off
import scala.concurrent.duration.Duration

import aoc2024.Day18.*
import util.Grid.Factory.given
import util.Position

// scalafix:on
class Day18Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  override val munitTimeout: Duration = scala.concurrent.duration.Duration(60, "s")

  test("Day18 - dummy".tag(ignore)):
    assert(true)

  test("Day18 - fromResource - test"):
    val (obtained, remaining) = fromResource("./inputs/Day18Test.txt", 12)

    assertEquals(obtained.free.size, 37)
    assertEquals(obtained.blocked.size, 12)
    assertEquals(obtained.start.get, Position(0, 0))
    assertEquals(obtained.end.get, Position(6, 6))
    assertEquals(obtained.dimensions, (7, 7))
    assertEquals(remaining.size, 13)

  test("Day18 - fromResource"):
    val (obtained, remaining) = fromResource("./inputs/Day18.txt", 1024)

    assertEquals(obtained.free.size, 4017)
    assertEquals(obtained.blocked.size, 1024)
    assertEquals(obtained.start.get, Position(0, 0))
    assertEquals(obtained.end.get, Position(70, 70))
    assertEquals(obtained.dimensions, (71, 71))
    assertEquals(remaining.size, 2426)

  test("Day18 - part1 - test"):
    val input = fromResource("./inputs/Day18Test.txt", 12)
    val obtained = part1(input)
    assertEquals(obtained, 22)

  test("Day18 - part1"):
    val input = fromResource("./inputs/Day18.txt", 1024)
    val obtained = part1(input)
    assertEquals(obtained, 318)

  test("Day18 - part2 - test"):
    val input = fromResource("./inputs/Day18Test.txt", 12)
    val obtained = part2(input)
    assertEquals(obtained, "(6,1)")

  test("Day18 - part2"):
    val input = fromResource("./inputs/Day18.txt", 1024)
    val obtained = part2(input)
    assertEquals(obtained, "(56,29)")

end Day18Test
