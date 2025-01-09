package aoc2024

import aoc2024.Day21.*

class Day21Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  val results = Map(
    "029A" -> "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",
    "980A" -> "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A",
    "179A" -> "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A",
    "456A" -> "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A",
    "379A" -> "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A",
  )

  test("Day21 - dummy".tag(ignore)):
    assert(true)

  test("Day21 - readFile - test"):
    val obtained = readFile("./inputs/Day21Test.txt")
    assertEquals(obtained.head, "179A")

  test("Day21 - readFile"):
    val obtained = readFile("./inputs/Day21.txt")
    assertEquals(obtained.head, "985A")

  test("Day21 - part1 - test"):
    val input = readFile("./inputs/Day21Test.txt")
    val obtained = part1(input)
    assertEquals(obtained, 4)

  test("Day21 - part1"):
    val input = readFile("./inputs/Day21.txt")
    val obtained = part1(input)
    assertEquals(obtained, 4)

  test("Day21 - part2 - test"):
    val input = readFile("./inputs/Day21Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 4)

  test("Day21 - part2"):
    val input = readFile("./inputs/Day21.txt")
    val obtained = part2(input)
    assertEquals(obtained, 4)

end Day21Test
