package aoc2024

import aoc2024.Day19._

class Day19Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day19 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day19 - readFile - test") {
    val obtained = readFile("./inputs/Day19Test.txt")
    assertEquals(obtained._1, Set("r", "wr", "b", "g", "bwu", "rb", "gb", "br"))
    assertEquals(obtained._2, Set("brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb"))
  }

  test("Day19 - readFile") {
    val obtained = readFile("./inputs/Day19.txt")
    assert(obtained._1.contains("wwubu"))
    assert(obtained._2.contains("gwwggrgbwbbguwgbguubbwrbgurgbburrgbwruwgurgwgguwbwgbgrr"))
  }

  test("Day19 - part1 - test") {
    val obtained = part1(readFile("./inputs/Day19Test.txt"))
    assertEquals(obtained, 6)
  }

  test("Day19 - part1") {
    val obtained = part1(readFile("./inputs/Day19.txt"))
    assertEquals(obtained, 278)
  }

  test("Day19 - part2 - test") {
    val obtained = part2(readFile("./inputs/Day19Test.txt"))
    assertEquals(obtained, 8)
  }

  test("Day19 - part2") {
    val obtained = part2(readFile("./inputs/Day19.txt"))
    assertEquals(obtained,400)
  }
}
