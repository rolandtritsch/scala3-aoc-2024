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

  test("Day18 - bfs - simple path") {
    val memory = Set.empty[Position]  // No obstacles
    val start = Position(0, 0)
    val end = Position(1, 1)
    
    val path = start.bfs(end, memory)
    assertEquals(path.map(_.size), Some(3))  // [0,0] -> [0,1] -> [1,1] or [0,0] -> [1,0] -> [1,1]
  }

  test("Day18 - bfs - blocked path") {
    val memory = Set(Position(1, 0), Position(0, 1))  // Block both possible paths
    val start = Position(0, 0)
    val end = Position(1, 1)
    
    val path = start.bfs(end, memory)
    assertEquals(path, None)  // No path possible
  }

  test("Day18 - bfs - finds shortest path") {
    val memory = Set(Position(1, 0))  // Block one path
    val start = Position(0, 0)
    val end = Position(1, 1)
    
    val path = start.bfs(end, memory)
    assertEquals(path.map(_.size), Some(3))  // Should still find path through [0,0] -> [0,1] -> [1,1]
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

  test("Day18 - part2 - test") {
    val input = readFile("./inputs/Day18Test.txt", 12)
    val obtained = part2(input, (7, 7))
    assertEquals(obtained, 12)
  }

  test("Day18 - part2") {
    val input = readFile("./inputs/Day18.txt", 1024)
    val obtained = part2(input, (71, 71))
    assertEquals(obtained, 1024)
  }
}
