package aoc2024

@main
def solve() = {
  println(s"Day00 - part1: ${Day00.part1(Day00.readFile("./inputs/Day00.txt"))}")
  println(s"Day00 - part2: ${Day00.part2(Day00.readFile("./inputs/Day00.txt"))}")

  println(s"Day01 - part1: ${Day01.part1(Day01.readFile("./inputs/Day01.txt"))}")
  println(s"Day01 - part2: ${Day01.part2(Day01.readFile("./inputs/Day01.txt"))}")

  println(s"Day02 - part1: ${Day02.part1(Day02.readFile("./inputs/Day02.txt"))}")
  println(s"Day02 - part2: ${Day02.part2(Day02.readFile("./inputs/Day02.txt"))}")

  println(s"Day03 - part1: ${Day03.part1(Day03.readFile("./inputs/Day03.txt"))}")
  println(s"Day03 - part2: ${Day03.part2(Day03.readFile("./inputs/Day03.txt"))}")

  println(s"Day04 - part1: ${Day04.part1(Day04.readFile("./inputs/Day04.txt"))}")
  println(s"Day04 - part2: ${Day04.part2(Day04.readFile("./inputs/Day04.txt"))}")

  val day05Rules = Day05.readFileRules("./inputs/Day05-Rules.txt")
  val day05Updates = Day05.readFileUpdates("./inputs/Day05-Updates.txt")
  println(s"Day05 - part1: ${Day05.part1(day05Rules, day05Updates)}")
  println(s"Day05 - part2: ${Day05.part2(day05Rules, day05Updates)}")

  println(s"Day06 - part1: ${Day06.part1(Day06.readFile("./inputs/Day06.txt"))}")
  println(s"Day06 - part2: ${Day06.part2(Day06.readFile("./inputs/Day06.txt"))}")

  println(s"Day07 - part1: ${Day07.part1(Day07.readFile("./inputs/Day07.txt"))}")
  println(s"Day07 - part2: ${Day07.part2(Day07.readFile("./inputs/Day07.txt"))}")

  println(s"Day08 - part1: ${Day08.part1(Day08.readFile("./inputs/Day08.txt"))}")
  println(s"Day08 - part2: ${Day08.part2(Day08.readFile("./inputs/Day08.txt"))}")

  println(s"Day09 - part1: ${Day09p1.part1(Day09p1.readFile("./inputs/Day09.txt"))}")
  println(s"Day09 - part2: ${Day09p2.part2(Day09p2.readFile("./inputs/Day09.txt"))}")

  println(s"Day10 - part1: ${Day10.part1(Day10.readFile("./inputs/Day10.txt"))}")
  println(s"Day10 - part2: ${Day10.part2(Day10.readFile("./inputs/Day10.txt"))}")

  println(s"Day11 - part1: ${Day11.part1(Day11.readFile("./inputs/Day11.txt"))}")
  println(s"Day11 - part2: ${Day11.part2(Day11.readFile("./inputs/Day11.txt"))}")

  println(s"Day12 - part1: ${Day12.part1(Day12.readFile("./inputs/Day12.txt"))}")
  println(s"Day12 - part2: ${Day12.part2(Day12.readFile("./inputs/Day12.txt"))}")

  println(s"Day13 - part1: ${Day13.part1(Day13.readFile("./inputs/Day13.txt"))}")
  println(s"Day13 - part2: ${Day13.part2(Day13.readFile("./inputs/Day13.txt", "1181818180"))}")

  println(s"Day14 - part1: ${Day14.part1(Day14.readFile("./inputs/Day14.txt"))}")
  println(s"Day14 - part2: ${Day14.part2(Day14.readFile("./inputs/Day14.txt"))}")

  val day15Warehose = Day15.readFileWarehouse("./inputs/Day15-Warehouse.txt")
  val day15Moves = Day15.readFileMoves("./inputs/Day15-Moves.txt")
  val day15State = (day15Warehose, day15Moves)
  println(s"Day15 - part1: ${Day15.part1(day15State)}")
  println(s"Day15 - part2: ${Day15.part2(day15State)}")

  println(s"Day16 - part1: ${Day16.part1(Day16.readFile("./inputs/Day16Test.txt"))}")
  println(s"Day16 - part2: ${Day16.part2(Day16.readFile("./inputs/Day16.txt"))}")

  val day17Registers = Day17.readFileRegisters("./inputs/Day17-Registers.txt")
  val day17Instructions = Day17.readFileInstructions("./inputs/Day17-Instructions.txt")
  val day17State = (day17Registers, day17Instructions)
  println(s"Day17 - part1: ${Day17.part1(day17State)}")
  println(s"Day17 - part2: ${Day17.part2(day17State)}")

  // println(s"Day18 - part1: ${Day18.part1(Day18.readFile("./inputs/Day18.txt", 1024), (71, 71))}")
  println(s"Day18 - part1: ${Day18.part1(Day18.readFile("./inputs/Day18Test.txt", 1024), (71, 71))}")
  println(s"Day18 - part2: ${Day18.part2(Day18.readFile("./inputs/Day18.txt", 1024), (71, 71))}")

  println(s"Day19 - part1: ${Day19.part1(Day19.readFile("./inputs/Day19.txt"))}")
  println(s"Day19 - part2: ${Day19.part2(Day19.readFile("./inputs/Day19.txt"))}")

  // println(s"Day20 - part1: ${Day20.part1(Day20.readFile("./inputs/Day20.txt"))}")
  println(s"Day20 - part1: ${Day20.part1(Day20.readFile("./inputs/Day20Test.txt"))}")
  println(s"Day20 - part2: ${Day20.part2(Day20.readFile("./inputs/Day20.txt"))}")

  println(s"Day21 - part1: ${Day21.part1(Day21.readFile("./inputs/Day21.txt"))}")
  println(s"Day21 - part2: ${Day21.part2(Day21.readFile("./inputs/Day21.txt"))}")

  println(s"Day22 - part1: ${Day22.part1(Day22.readFile("./inputs/Day22.txt"))}")
  println(s"Day22 - part2: ${Day22.part2(Day22.readFile("./inputs/Day22.txt"))}")

  println(s"Day23 - part1: ${Day23.part1(Day23.readFile("./inputs/Day23.txt"))}")
  println(s"Day23 - part2: ${Day23.part2(Day23.readFile("./inputs/Day23.txt"))}")

  val day24Initials = Day24.readFileInitials("./inputs/Day24-Initials.txt")
  val day24Statements = Day24.readFileStatements("./inputs/Day24-Statements.txt")
  val day24State = (day24Initials, day24Statements)
  println(s"Day24 - part1: ${Day24.part1(day24State)}")
  println(s"Day24 - part2: ${Day24.part2(day24State)}")
}
