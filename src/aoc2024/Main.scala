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

  println(s"Day05 - part1: ${Day05.part1(Day05.readFileRules("./inputs/Day05-Rules.txt"), Day05.readFileUpdates("./inputs/Day05-Updates.txt"))}")
  println(s"Day05 - part2: ${Day05.part2(Seq(1))}")
}
