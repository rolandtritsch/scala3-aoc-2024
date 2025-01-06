package aoc2024

import aoc2024.Day20._

class Day20Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day20 - dummy".tag(ignore)):
    assert(true)

  test("Day20 - readFile - test"):
    val obtained = readFile("./inputs/Day20Test.txt")
    val (track, program) = obtained
    assertEquals(track.walls.size, 140)
    assertEquals(program, Position(3, 1))

  test("Day20 - readFile"):
    val obtained = readFile("./inputs/Day20.txt")
    val (track, program) = obtained
    assertEquals(track.walls.size, 10560)
    assertEquals(program, Position(67, 75))

  test("Day20 - dfs - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input
    Day20.visited.clear()
    val shortestPath = program.dfs(track)
    assertEquals(shortestPath.get.size, 84)

  test("Day20 - dfs - test2"):
    val input = readFile("./inputs/Day20Test2.txt")
    val (track, program) = input
    Day20.visited.clear()
    val shortestPath = program.dfs(track)
    assertEquals(shortestPath.get.size, 72)

  // test("Day20 - dfs") {
  //   val input = readFile("./inputs/Day20.txt")
  //   val (track, program) = input
  //   Day20.visited.clear()
  //   val shortestPath = program.dfs(track)
  //   assertEquals(shortestPath.get.size, 9320)
  // }

  test("Day20 - shortCuts - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input

    Day20.visited.clear()
    val shortestPath = program.dfs(track)

    val shortCuts = track.shortCuts(program, shortestPath.get)

    assertEquals(shortCuts.size, 44)

  test("Day20 - shortCuts - shortCutsPathLength - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input

    Day20.visited.clear()
    val shortestPath = program.dfs(track)
    val shortestPathLength = shortestPath.get.length

    val shortCutsPathLength = track.shortCuts(program, shortestPath.get).map { case (c, p) => {
      (c, p.getOrElse(List.empty).length)
    }}.toList

    assertEquals(shortCutsPathLength.size, 44)

  test("Day20 - shortCuts - shortCutsValue - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input

    Day20.visited.clear()
    val shortestPath = program.dfs(track)
    val shortestPathLength = shortestPath.get.length

    val shortCutsPathLength = track.shortCuts(program, shortestPath.get).map { case (c, p) => {
      (c, p.getOrElse(List.empty).length)
    }}.toList

    val shortCutsValue = shortCutsPathLength.map { (c, p) => {
      (c, shortestPathLength - p)
    }}

    assertEquals(shortCutsValue.size, 44)

  test("Day20 - shortCuts - final - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val (track, program) = input

    Day20.visited.clear()
    val shortestPath = program.dfs(track)
    val shortestPathLength = shortestPath.get.length

    val shortCutsPathLength = track.shortCuts(program, shortestPath.get).map { case (c, p) => {
      (c, p.getOrElse(List.empty).length)
    }}.toList

    val shortCutsValue = shortCutsPathLength.map { (c, p) => {
      (c, shortestPathLength - p)
    }}

    val obtained = shortCutsValue.groupBy(_._2).map { (cheatValue, cheatList) => {
      (cheatValue, cheatList.size)
    }}.toSet

    val expected = Set(
      (14, 2),
      (14, 4),
      (2, 6),
      (4, 8),
      (2, 10),
      (3, 12),
      (1, 20),
      (1, 36),
      (1, 38),
      (1, 40),
      (1, 64),
    ).map((c, v) => (v, c))

    assertEquals(obtained, expected)

  test("Day20 - part1 - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part1(input, 50)
    assertEquals(obtained, 1)

  test("Day20 - part1"):
    // val input = readFile("./inputs/Day20.txt")
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 0)

  test("Day20 - part2 - test"):
    val input = readFile("./inputs/Day20Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 140)

  test("Day20 - part2"):
    val input = readFile("./inputs/Day20.txt")
    val obtained = part2(input)
    assertEquals(obtained, 10560)
