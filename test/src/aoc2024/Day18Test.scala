package aoc2024

import aoc2024.Day18._

class Day18Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  test("Day18 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day18 - readFile - test") {
    val obtained = readFile("./inputs/Day18Test.txt")
    assertEquals(obtained.head, Position(5, 2))
  }

  test("Day18 - readFile") {
    val obtained = readFile("./inputs/Day18.txt")
    assertEquals(obtained.head, Position(50, 16))
  }

  test("Day18 - part1 - test") {
    val input = readFile("./inputs/Day18Test.txt", 12)
    val obtained = part1(input, (7, 7))
    assertEquals(obtained, 22)
  }

  test("Day18 - part1") {
    val input = readFile("./inputs/Day18.txt", 1024)
    val obtained = part1(input, (71, 71))
    assertEquals(obtained, 318)
  }

//   test("Day18 - part2 - test") {
//     val input = readFile("./inputs/Day18Test.txt", 12)
//     val obtained = part2(input, (7, 7))
//     assertEquals(obtained, 12)
//   }

//   test("Day18 - part2") {
//     val input = readFile("./inputs/Day18.txt", 1024)
//     val obtained = part2(input, (71, 71))
//     assertEquals(obtained, 1024)
//   }
}
