package aoc2024

import aoc2024.Day17._

class Day17Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day17 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day17 - readFileRegisters - test") {
    val registers = readFileRegisters("./inputs/Day17Test-Registers.txt")
    assertEquals(registers('A'), 729)
  }

  test("Day17 - readFileRegisters") {
    val registers = readFileRegisters("./inputs/Day17-Registers.txt")
    assertEquals(registers('A'), 46337277)
  }

  test("Day17 - readFileInstructions - test") {
    val instructions = readFileInstructions("./inputs/Day17Test-Instructions.txt")
    assertEquals(instructions(0), ADV(1))
  }

  test("Day17 - readFileInstructions") {
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    assertEquals(instructions(0), BST(4))
  }

  test("Day17 - part1 - test") {
    val registers = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained = part1(registers, instructions)
    assertEquals(obtained, 0)
  }

  test("Day17 - part1") {
    val registers = readFileRegisters("./inputs/Day17-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    val obtained = part1(registers, instructions)
    assertEquals(obtained, 0)
  }

  test("Day17 - part2 - test") {
    val registers = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained = part2(registers, instructions)
    assertEquals(obtained, 0)
  }

  test("Day17 - part2") {
    val registers = readFileRegisters("./inputs/Day17-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    val obtained = part2(registers, instructions)
    assertEquals(obtained, 0)
  }
}
