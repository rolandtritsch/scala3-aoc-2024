package aoc2024

import aoc2024.Day17.*

class Day17Test extends munit.ScalaCheckSuite:
  val only   = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day17 - dummy".tag(ignore)):
    assert(true)

  test("Day17 - readFileRegisters - test"):
    val registers = readFileRegisters("./inputs/Day17Test-Registers.txt")
    assertEquals(registers('A'), 729L)

  test("Day17 - readFileRegisters"):
    val registers = readFileRegisters("./inputs/Day17-Registers.txt")
    assertEquals(registers('A'), 46337277L)

  test("Day17 - instruction - ADV"):
    val instruction = Instruction.create(0, 5)
    val registers   = Map('A' -> 15360L, 'B' -> 10L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('A'), 15L)

  test("Day17 - instruction - BXL"):
    val instruction = Instruction.create(1, 3)
    val registers   = Map('B' -> 4L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('B'), 7L)

  test("Day17 - instruction - BST"):
    val instruction = Instruction.create(2, 4)
    val registers   = Map('A' -> 511L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('B'), 7L)

  test("Day17 - instruction - JNZ - no jump"):
    val instruction = Instruction.create(3, 999)
    val registers   = Map('A' -> 0L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained.contains('Z'), false)

  test("Day17 - instruction - JNZ - jump"):
    val instruction = Instruction.create(3, 999)
    val registers   = Map('A' -> 1L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('Z'), 999L)

  test("Day17 - instruction - BXC"):
    val instruction = Instruction.create(4, 0)
    val registers   = Map('B' -> 4L, 'C' -> 3L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('B'), 7L)

  test("Day17 - instruction - BXC"):
    val instruction = Instruction.create(4, 0)
    val registers   = Map('B' -> 2024L, 'C' -> 43690L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('B'), 44354L)

  test("Day17 - instruction - OUT"):
    val instruction = Instruction.create(5, 4)
    val registers   = Map('A' -> 511L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('Y'), 7L)

  test("Day17 - instruction - BDV"):
    val instruction = Instruction.create(6, 5)
    val registers   = Map('A' -> 15360L, 'B' -> 10L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('B'), 15L)

  test("Day17 - instruction - CDV"):
    val instruction = Instruction.create(7, 5)
    val registers   = Map('A' -> 15360L, 'B' -> 10L)
    val obtained    = instruction.execute(registers)
    assertEquals(obtained('C'), 15L)

  test("Day17 - readFileInstructions - test"):
    val instructions =
      readFileInstructions("./inputs/Day17Test-Instructions.txt")
    assertEquals(instructions(0), ADV(1))

  test("Day17 - readFileInstructions"):
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    assertEquals(instructions(0), BST(4))

  test("Day17 - Program - next"):
    val registers    = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions =
      readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained     = new Program(0, registers, instructions, List.empty)
    assertEquals(obtained.counter, 0)
    assertEquals(obtained.registers('A'), 729L)
    assertEquals(obtained.instructions(0), ADV(1))

    val obtained2 = obtained.next
    assertEquals(obtained2.counter, 1)
    assertEquals(obtained2.registers('A'), 364L)
    assertEquals(obtained2.instructions(obtained2.counter), OUT(4))

  test("Day17 - part1 - test3"):
    val registers    = readFileRegisters("./inputs/Day17Test3-Registers.txt")
    val instructions =
      readFileInstructions("./inputs/Day17Test3-Instructions.txt")
    val obtained     = part1(registers, instructions)
    assertEquals(obtained, "4,2,5,6,7,7,7,7,3,1,0")

  test("Day17 - part1 - test2"):
    val registers    = readFileRegisters("./inputs/Day17Test2-Registers.txt")
    val instructions =
      readFileInstructions("./inputs/Day17Test2-Instructions.txt")
    val obtained     = part1(registers, instructions)
    assertEquals(obtained, "0,1,2")

  test("Day17 - part1 - test"):
    val registers    = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions =
      readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained     = part1(registers, instructions)
    assertEquals(obtained, "4,6,3,5,6,3,5,2,1,0")

  test("Day17 - part1"):
    val registers    = readFileRegisters("./inputs/Day17-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    val obtained     = part1(registers, instructions)
    assertEquals(obtained, "7,4,2,0,5,0,5,3,7")

  test("Day17 - part2 - test"):
    val registers    = readFileRegisters("./inputs/Day17Test-Registers.txt")
    val instructions =
      readFileInstructions("./inputs/Day17Test-Instructions.txt")
    val obtained     = part2(registers, instructions)
    assertEquals(obtained, 0)

  test("Day17 - part2"):
    val registers    = readFileRegisters("./inputs/Day17-Registers.txt")
    val instructions = readFileInstructions("./inputs/Day17-Instructions.txt")
    val obtained     = part2(registers, instructions)
    assertEquals(obtained, 0)
