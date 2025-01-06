package aoc2024

import aoc2024.Day15._

class Day15Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day15 - dummy".tag(ignore)):
    assert(true)

  test("Day15 - readFile - Warehouse - test"):
    val obtained = readFileWarehouse("./inputs/Day15Test-Warehouse.txt")
    assertEquals(obtained.robot, Position(4, 4))

  test("Day15 - readFile - Moves - test"):
    val obtained = readFileMoves("./inputs/Day15Test-Moves.txt")
    assertEquals(obtained.size, 700)

  test("Day15 - readFile - Warehouse"):
    val obtained = readFileWarehouse("./inputs/Day15-Warehouse.txt")
    assertEquals(obtained.robot, Position(24, 24))

  test("Day15 - readFile - Moves"):
    val obtained = readFileMoves("./inputs/Day15-Moves.txt")
    assertEquals(obtained.size, 20000)

  test("Day15 - move"):
    val wh = readFileWarehouse("./inputs/Day15Test2-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15Test2-Moves.txt")
    val obtained = wh.move(moves.head)
    assertEquals(obtained.robot, Position(2, 2))

  test("Day15 - states"):
    val wh = readFileWarehouse("./inputs/Day15Test2-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15Test2-Moves.txt")
    val obtained = (wh, moves.take(4)).finalState._1
    assertEquals(obtained.robot, Position(1, 3))

  test("Day15 - part1 - test2"):
    val wh = readFileWarehouse("./inputs/Day15Test2-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15Test2-Moves.txt")
    val obtained = part1((wh, moves))
    assertEquals(obtained, 2028)

  test("Day15 - part1 - test"):
    val wh = readFileWarehouse("./inputs/Day15Test-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15Test-Moves.txt")
    val obtained = part1((wh, moves))
    assertEquals(obtained, 10092)

  test("Day15 - part1"):
    val wh = readFileWarehouse("./inputs/Day15-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15-Moves.txt")
    val obtained = part1((wh, moves))
    assertEquals(obtained, 1463512)

  test("Day15 - part2 - test"):
    val wh = readFileWarehouse("./inputs/Day15Test-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15Test-Moves.txt")
    val obtained = part2((wh, moves))
    assertEquals(obtained, 4)

  test("Day15 - part2"):
    val wh = readFileWarehouse("./inputs/Day15-Warehouse.txt")
    val moves = readFileMoves("./inputs/Day15-Moves.txt")
    val obtained = part2((wh, moves))
    assertEquals(obtained, 24)
