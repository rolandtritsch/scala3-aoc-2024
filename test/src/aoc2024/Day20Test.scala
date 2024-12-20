package aoc2024

import aoc2024.Day20._

class Day20Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day20 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day20 - readFile - test") {
    val obtained = readFile("./inputs/Day20Test.txt")
    val (track, program) = obtained
    assertEquals(track.walls.size, 140)
    assertEquals(program, Position(3, 1))
  }

  test("Day20 - readFile") {
    val obtained = readFile("./inputs/Day20.txt")
    val (track, program) = obtained
    assertEquals(track.walls.size, 10560)
    assertEquals(program, Position(67, 75))
  }

  test("Day20 - dfs - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input
    val (visited, shortestPath) = program.dfs(track.end, track.walls)
    assertEquals(shortestPath.get.size, 84)
  }

  test("Day20 - dfs - test2") {
    val input = readFile("./inputs/Day20Test2.txt")
    val (track, program) = input
    val (visited, shortestPath) = program.dfs(track.end, track.walls)
    assertEquals(shortestPath.get.size, 72)
  }

  test("Day20 - dfs") {
    val input = readFile("./inputs/Day20.txt")
    val (track, program) = input
    val (visited, shortestPath) = program.dfs(track.end, track.walls)
    assertEquals(shortestPath.get.size, 9320)
  }

    test("Day20 - shortCuts - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input
    val shortCuts = track.shortCuts(program)
    assertEquals(shortCuts.size, 135)
  }

  test("Day20 - shortCuts - shortCutsPathLength - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input

    val (_, shortestPath) = program.dfs(track.end, track.walls)
    val shortestPathLength = shortestPath.get.length

    val shortCutsPathLength = track.shortCuts(program).map { case (c, p) => {
      (c, p.getOrElse(List.empty).length)
    }}.toList

    assertEquals(shortCutsPathLength.size, 135)
  }

  test("Day20 - shortCuts - shortCutsValue - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input

    val (_, shortestPath) = program.dfs(track.end, track.walls)
    val shortestPathLength = shortestPath.get.length

    val shortCutsPathLength = track.shortCuts(program).map { case (c, p) => {
      (c, p.getOrElse(List.empty).length)
    }}.toList

    val shortCutsValue = shortCutsPathLength.map { (c, p) => {
      (c, shortestPathLength - p)
    }}
    println(shortCutsValue.sortBy(_._2).reverse)

    assertEquals(shortCutsValue.size, 135)
  }

  // test("Day20 - shortCuts - final - test") {
  //   val input = readFile("./inputs/Day20Test.txt")
  //   val (track, program) = input

  //   val (_, shortestPath) = program.dfs(track.end, track.walls)
  //   val shortestPathLength = shortestPath.get.length

  //   val shortCutsPathLength = track.shortCuts(program).map { case (c, p) => {
  //     (c, p.getOrElse(List.empty).length)
  //   }}.toList

  //   val shortCutsValue = shortCutsPathLength.map { (c, p) => {
  //     (c, shortestPathLength - p)
  //   }}

  //   val obtained =shortCutsValue.groupBy(_._2).map { (cheatValue, cheatList) => {
  //     (cheatList.size, cheatValue)
  //   }}.filter(_._2 > 0).toSet

  //   val expected = Set(
  //     (14, 2),
  //     (14, 4),
  //     (2, 6),
  //     (4, 8),
  //     (2, 10),
  //     (3, 12),
  //     (1, 20),
  //     (1, 36),
  //     (1, 38),
  //     (1, 40),
  //     (1, 64),
  //   )

  //   assertEquals(obtained, expected)
  // }

  // test("Day20 - part1 - test") {
  //   val input = readFile("./inputs/Day20Test.txt")
  //   val obtained = part1(input)
  //   assertEquals(obtained, 140)
  // }

  // test("Day20 - part1") {
  //   val input = readFile("./inputs/Day20.txt")
  //   val obtained = part1(input)
  //   assertEquals(obtained, 10560)
  // }

  test("Day20 - part2 - test") {
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 140)
  }

  test("Day20 - part2") {
    val input = readFile("./inputs/Day20.txt")
    val obtained = part2(input)
    assertEquals(obtained, 10560)
  }
}
