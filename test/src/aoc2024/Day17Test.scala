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
    assertEquals(registers('A'), BigInt(729))
  }

  test("Day17 - readFileRegisters") {
    val registers = readFileRegisters("./inputs/Day17-Registers.txt")
    assertEquals(registers('A'), BigInt(46337277))
  }

  test("Day17 - instruction - ADV") {
    val instruction = Instruction.create(0, 5)
    val registers = Map('A' -> BigInt(15360), 'B' -> BigInt(10))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('A'), BigInt(15))
  }

  test("Day17 - instruction - BXL") {
    val instruction = Instruction.create(1, 4)
    val registers = Map('A' -> BigInt(3), 'B' -> BigInt(4))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('B'), BigInt(7))
  }

  test("Day17 - instruction - BST") {
    val instruction = Instruction.create(2, 4)
    val registers = Map('A' -> BigInt(511))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('B'), BigInt(7))
  }

  test("Day17 - instruction - JNZ - no jump") {
    val instruction = Instruction.create(3, 5)
    val registers = Map('A' -> BigInt(0), 'B' -> BigInt(999))
    val obtained = instruction.execute(registers)
    assertEquals(obtained.getOrElse('Z', BigInt(Int.MinValue)), BigInt(Int.MinValue))
  }

  test("Day17 - instruction - JNZ - jump") {
    val instruction = Instruction.create(3, 5)
    val registers = Map('A' -> BigInt(1), 'B' -> BigInt(999))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('Z'), BigInt(999))
  }

  test("Day17 - instruction - BXC") {
    val instruction = Instruction.create(4, 0)
    val registers = Map('B' -> BigInt(4), 'C' -> BigInt(3))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('B'), BigInt(7))
  }

  test("Day17 - instruction - BXC") {
    val instruction = Instruction.create(4, 0)
    val registers = Map('B' -> BigInt(2024), 'C' -> BigInt(43690))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('B'), BigInt(44354))
  }

  test("Day17 - instruction - OUT") {
    val instruction = Instruction.create(5, 4)
    val registers = Map('A' -> BigInt(511))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('Y'), BigInt(7))
  }

  test("Day17 - instruction - BDV") {
    val instruction = Instruction.create(6, 5)
    val registers = Map('A' -> BigInt(15360), 'B' -> BigInt(10))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('B'), BigInt(15))
  }

  test("Day17 - instruction - CDV") {
    val instruction = Instruction.create(7, 5)
    val registers = Map('A' -> BigInt(15360), 'B' -> BigInt(10))
    val obtained = instruction.execute(registers)
    assertEquals(obtained('C'), BigInt(15))
  }

  test("Day17 - readFileInstructions - test") {
    val instructions = readFileInstructions("./inputs/Day17Test-Instructions.txt")
    assertEquals(instructions(0), ADV(1))
  }

  test("Day17 - readFileInstructions") {
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    assertEquals(instructions(0), BST(4))
  }

  test("Day17 - Program - next") {
    val registers = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained = new Program(0, registers, instructions, List.empty)
    assertEquals(obtained.counter, 0)
    assertEquals(obtained.registers('A'), BigInt(729))
    assertEquals(obtained.instructions(0), ADV(1))

    val obtained2 = obtained.next
    assertEquals(obtained2.counter, 1)
    assertEquals(obtained2.registers('A'), BigInt(364))
    assertEquals(obtained2.instructions(obtained2.counter), OUT(4))
  }

  test("Day17 - part1 - test3") {
    val registers = readFileRegisters("./inputs/Day17Test3-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17Test3-Instructions.txt")
    val obtained = part1(registers, instructions)
    assertEquals(obtained, "4,2,5,6,7,7,7,7,3,1,0")
  }

  test("Day17 - part1 - test2") {
    val registers = readFileRegisters("./inputs/Day17Test2-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17Test2-Instructions.txt")
    val obtained = part1(registers, instructions)
    assertEquals(obtained, "0,1,2")
  }

  test("Day17 - part1 - test") {
    val registers = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained = part1(registers, instructions)
    assertEquals(obtained, "4,6,3,5,6,3,5,2,1,0")
  }

  test("Day17 - part1") {
    val registers = readFileRegisters("./inputs/Day17-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    val obtained = part1(registers, instructions)
    assertEquals(obtained, "6,7,5,2,1,2,1,1,1")
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
