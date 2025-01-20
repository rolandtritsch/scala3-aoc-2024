package aoc2024

import aoc2024.Day21.*

class Day21Test extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  val results: Map[String, String] = Map(
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

  test("Day21 - next(029A)"):
    val pad = NumericKeypad.create
    val keys = pad.path("029A")
    val obtained = next(keys, 'A', "", Set())
    val expected = "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
    assertEquals(obtained.size, 8)
    assertEquals(obtained.map(_.size), Set(28))
    assert(obtained.contains(expected))

    val obtained2 = obtained.flatMap(n => next(n, 'A', "", Set()))
    assertEquals(obtained2.size, 512)
    assertEquals(obtained2.map(_.size), Set(results("029A").size))
    // assert(obtained2.contains(results("029A")))

  // test("Day21 - next(179A)"):
  //   val pad = NumericKeypad.create
  //   val keys = pad.path("179A")
  //   val obtained = next(keys, 'A', "", Set())
  //   val expected = "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
  //   assertEquals(obtained.size, 4)
  //   assertEquals(obtained.map(_.size), Set(32))
  //   // assert(obtained.contains(expected))

  //   val obtained2 = obtained.flatMap(n => next(n, 'A', "", Set()))
  //   val expected2 = "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
  //   assertEquals(obtained2.size, 1024)
  //   assertEquals(obtained2.map(_.size), Set(68))
  //   //assert(obtained2.contains(expected2))

  test("Day21 - level"):
    val obtained = level("<A^A>^^AvvvA", 1)
    assertEquals(obtained, 68)

  test("Day21 - path0"):
    val pad = NumericKeypad.create
    assertEquals(pad.path0('A', '0'), "<")
    assertEquals(pad.path0('0', '2'), "^")
    assertEquals(pad.path0('2', '9'), ">^^")
    assertEquals(pad.path0('9', 'A'), "vvv")

  test("Day21 - path"):
    val pad = NumericKeypad.create
    val obtained = pad.path("029A")
    val expected = "<A^A>^^AvvvA"
    assertEquals(obtained, expected)

  test("Day21 - complexity"):
    val complexities = results.map((code, keys) => complexity(code, keys.length))
    assertEquals(complexities.sum, 126384)

  // test("Day21 - part1 - test"):
  //   val input = readFile("./inputs/Day21Test.txt")
  //   val obtained = part1(input)
  //   assertEquals(obtained, 126384)

  // test("Day21 - part1"):
  //   val input = readFile("./inputs/Day21.txt")
  //   val obtained = part1(input)
  //   assertEquals(obtained, 4)

  test("Day21 - part2 - test"):
    val input = readFile("./inputs/Day21Test.txt")
    val obtained = part2(input)
    assertEquals(obtained, 4)

  test("Day21 - part2"):
    val input = readFile("./inputs/Day21.txt")
    val obtained = part2(input)
    assertEquals(obtained, 4)

end Day21Test
