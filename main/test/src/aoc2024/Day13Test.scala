package aoc2024

class Day13Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  import aoc2024.Day13._

  test("Day13 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day13 - readFile - test") {
    val obtained = Day13.readFile("./inputs/Day13Test.txt")
    val expected = ClawMachine(
      Button(Position(94, 34), 3),
      Button(Position(22, 67), 1),
      Position(8400, 5400)
    )
    assertEquals(obtained.head, expected)
  }

  test("Day13 - readFile - test") {
    val obtained = Day13.readFile("./inputs/Day13Test.txt", "1000000000")
    val expected = ClawMachine(
      Button(Position(94, 34), 3),
      Button(Position(22, 67), 1),
      Position(10000000008400L, 10000000005400L)
    )
    assertEquals(obtained.head, expected)
  }

  test("Day13 - readFile") {
    val obtained = Day13.readFile("./inputs/Day13.txt")
    assertEquals(obtained.size, 320)
  }

  test("Day13 - ordering") {
    import scala.math.Ordering.Implicits._

    assert(Position(0, 1) > Position(0, 0))
    assert(Position(1, 0) > Position(0, 0))
    assert(Position(1, 1) > Position(0, 0))
    assert(Position(0, 0) == Position(0, 0))
  }

  test("Day13 - chepeastWayToWin") {
    val machine = ClawMachine(
      Button(Position(2, 2), 3),
      Button(Position(1, 1), 1),
      Position(10, 10)
    )
    val obtained = machine.cheapestWayToWin(10)

    assertEquals(obtained, Some(10L))
  }

  test("Day13 - chepeastWayToWin - test") {
    val machines = Day13.readFile("./inputs/Day13Test.txt")
    val obtained = machines.head.cheapestWayToWin(200)

    assertEquals(obtained, Some(280L))
  }

  test("Day13 - part1 - test") {
    val input = Day13.readFile("./inputs/Day13Test.txt")
    val obtained = Day13.part1(input)
    assertEquals(obtained, 480L)
  }

  test("Day13 - part1") {
    val input = Day13.readFile("./inputs/Day13.txt")
    val obtained = Day13.part1(input)
    assertEquals(obtained, 34393L)
  }

  test("Day13 - part2 - test") {
    val input = Day13.readFile("./inputs/Day13Test.txt", "1000000000")
    val obtained = Day13.part2(input)
    assertEquals(obtained, 0L)
  }

  test("Day13 - part2") {
    val input = Day13.readFile("./inputs/Day13.txt", "1000000000")
    val obtained = Day13.part2(input)
    assertEquals(obtained, 0L)
  }
}
