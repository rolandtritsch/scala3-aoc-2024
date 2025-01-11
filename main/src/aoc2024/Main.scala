package aoc2024

@main
def solve() =

  val input00 = Day00.readFile("./inputs/Day00.txt")
  println(s"Day00 - part1: ${Day00.part1(input00)}")
  println(s"Day00 - part2: ${Day00.part2(input00)}")

  val input01 = Day01.readFile("./inputs/Day01.txt")
  println(s"Day01 - part1: ${Day01.part1(input01)}")
  println(s"Day01 - part2: ${Day01.part2(input01)}")

  val input02 = Day02.readFile("./inputs/Day02.txt")
  println(s"Day02 - part1: ${Day02.part1(input02)}")
  println(s"Day02 - part2: ${Day02.part2(input02)}")

  val input03 = Day03.readFile("./inputs/Day03.txt")
  println(s"Day03 - part1: ${Day03.part1(input03)}")
  println(s"Day03 - part2: ${Day03.part2(input03)}")

  val input04 = Day04.readFile("./inputs/Day04.txt")
  println(s"Day04 - part1: ${Day04.part1(input04)}")
  println(s"Day04 - part2: ${Day04.part2(input04)}")

  val input05Rules = Day05.readFileRules("./inputs/Day05-Rules.txt")
  val input05Updates = Day05.readFileUpdates("./inputs/Day05-Updates.txt")
  println(s"Day05 - part1: ${Day05.part1(input05Rules, input05Updates)}")
  println(s"Day05 - part2: ${Day05.part2(input05Rules, input05Updates)}")

  val input06p1 = Day06.readFile("./inputs/Day06.txt")
  val input06p2 = Day06.readFile("./inputs/Day06.txt")
  println(s"Day06 - part1: ${Day06.part1(input06p1)}")
  println(s"Day06 - part2: ${Day06.part2(input06p2)}")

  val input07 = Day07.readFile("./inputs/Day07.txt")
  println(s"Day07 - part1: ${Day07.part1(input07)}")
  println(s"Day07 - part2: ${Day07.part2(input07)}")

  val input08 = Day08.readFile("./inputs/Day08.txt")
  println(s"Day08 - part1: ${Day08.part1(input08)}")
  println(s"Day08 - part2: ${Day08.part2(input08)}")

  val input09p1 = Day09p1.readFile("./inputs/Day09.txt")
  val input09p2 = Day09p2.readFile("./inputs/Day09.txt")
  println(s"Day09 - part1: ${Day09p1.part1(input09p1)}")
  println(s"Day09 - part2: ${Day09p2.part2(input09p2)}")

  val input10 = Day10.readFile("./inputs/Day10.txt")
  println(s"Day10 - part1: ${Day10.part1(input10)}")
  println(s"Day10 - part2: ${Day10.part2(input10)}")

  val input11 = Day11.readFile("./inputs/Day11.txt")
  println(s"Day11 - part1: ${Day11.part1(input11)}")
  println(s"Day11 - part2: ${Day11.part2(input11)}")

  val input12 = Day12.readFile("./inputs/Day12.txt")
  println(s"Day12 - part1: ${Day12.part1(input12)}")
  println(s"Day12 - part2: ${Day12.part2(input12)}")

  val input13 = Day13.readFile("./inputs/Day13.txt")
  println(s"Day13 - part1: ${Day13.part1(input13)}")
  println(s"Day13 - part2: ${Day13.part2(input13)}")

  val input14 = Day14.readFile("./inputs/Day14.txt")
  println(s"Day14 - part1: ${Day14.part1(input14)}")
  println(s"Day14 - part2: ${Day14.part2(input14)}")

  val input15Warehouse = Day15.readFileWarehouse("./inputs/Day15-Warehouse.txt")
  val input15Moves = Day15.readFileMoves("./inputs/Day15-Moves.txt")
  val input15State = (input15Warehouse, input15Moves)
  println(s"Day15 - part1: ${Day15.part1(input15State)}")
  println(s"Day15 - part2: ${Day15.part2(input15State)}")

  val input16 = Day16.readFile("./inputs/Day16.txt")
  println(s"Day16 - part1: ${Day16.part1(input16)}")
  println(s"Day16 - part2: ${Day16.part2(input16)}")

  val input17Registers = Day17.readFileRegisters("./inputs/Day17-Registers.txt")
  val input17Instructions = Day17.readFileInstructions("./inputs/Day17-Instructions.txt")
  val input17State = (input17Registers, input17Instructions)
  println(s"Day17 - part1: ${Day17.part1(input17State)}")
  println(s"Day17 - part2: ${Day17.part2(input17State)}")

  val input18p1 = Day18.readFile("./inputs/Day18.txt")
  val input18p2 = Day18.readFile("./inputs/Day18.txt")
  println(s"Day18 - part1: ${Day18.part1(input18p1)}")
  println(s"Day18 - part2: ${Day18.part2(input18p2)}")

  val input19 = Day19.readFile("./inputs/Day19Test.txt")
  println(s"Day19 - part1: ${Day19.part1(input19)}")
  println(s"Day19 - part2: ${Day19.part2(input19)}")

  val input20 = Day20.readFile("./inputs/Day20Test.txt")
  println(s"Day20 - part1: ${Day20.part1(input20)}")
  println(s"Day20 - part2: ${Day20.part2(input20)}")

  val input21 = Day21.readFile("./inputs/Day21Test.txt")
  println(s"Day21 - part1: ${Day21.part1(input21)}")
  println(s"Day21 - part2: ${Day21.part2(input21)}")

  val input22 = Day22.readFile("./inputs/Day22Test.txt")
  println(s"Day22 - part1: ${Day22.part1(input22)}")
  println(s"Day22 - part2: ${Day22.part2(input22)}")

  val input23 = Day23.readFile("./inputs/Day23Test.txt")
  println(s"Day23 - part1: ${Day23.part1(input23)}")
  println(s"Day23 - part2: ${Day23.part2(input23)}")

  val input24Initials = Day24.readFileInitials("./inputs/Day24-Initials.txt")
  val input24Statements = Day24.readFileStatements("./inputs/Day24-Statements.txt")
  val input24State = (input24Initials, input24Statements)
  println(s"Day24 - part1: ${Day24.part1(input24State)}")
  println(s"Day24 - part2: ${Day24.part2(input24State)}")

end solve
