package aoc2024
// scalafix: off
import aoc2024.Day16.*
import aoc2024.Day16.Implicits.given
import util.Grid
import util.Grid.Factory.given
import util.WDGridGraph
// scalafix: on
class Day16Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day16 - dummy".tag(ignore)):
    assert(true)

  test("Day16 - fromResource - test"):
    val grid = Grid.fromResource("./inputs/Day16Test.txt")
    val graph = WDGridGraph.fromGrid(grid)
    assertEquals(graph.nodes.size, 416)
    assertEquals(graph.edges.size, 660)

  test("Day16 - part1 - test"):
    val input = readFile("./inputs/Day16Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 7036)

  test("Day16 - part1 - test2"):
    val input = readFile("./inputs/Day16Test2.txt")
    val obtained = part1(input)
    assertEquals(obtained, 11048)

  test("Day16 - part1"):
    val input = readFile("./inputs/Day16.txt")
    val obtained = part1(input)
    assertEquals(obtained, 78428)

  test("Day16 - part2 - test"):
    val input = readFile("./inputs/Day16Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 416)

  test("Day16 - part2"):
    val input = readFile("./inputs/Day16.txt")
    val obtained = part2(input)
    assertEquals(obtained, 40504)

end Day16Test
