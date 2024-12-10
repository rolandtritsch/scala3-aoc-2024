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

  println(s"Day09 - part1: ${Day09.part1(Day09.readFile("./inputs/Day09.txt"))}")
  println(s"Day09 - part2: ${Day09.part2(Day09.readFile2("./inputs/Day09.txt"))}")

  println(s"Day10 - part1: ${Day10.part1(Day10.readFile("./inputs/Day10.txt"))}")
  println(s"Day10 - part2: ${Day10.part2(Day10.readFile("./inputs/Day10.txt"))}")
}
