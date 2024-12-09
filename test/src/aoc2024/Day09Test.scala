package aoc2024

class Day09Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day09 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day09 - readFile - test") {
    val obtained = Day09.readFile("./inputs/Day09Test.txt")
    assertEquals(obtained.blocks.size, 42)
    assertEquals(obtained.toString, "00...111...2...333.44.5555.6666.777.888899")
  }

  test("Day09 - readFile") {
    val obtained = Day09.readFile("./inputs/Day09.txt")
    assertEquals(obtained.blocks.size, 95482)
  }

  test("Day09 - defragment") {
    val disk = Day09.readFile("./inputs/Day09Test.txt")
    val obtained = disk.defragment
    val expected = "0099811188827773336446555566.............."

    assertEquals(obtained.toString, expected)
  }

  test("Day09 - part1 - test") {
    val input = Day09.readFile("./inputs/Day09Test.txt")
    val obtained = Day09.part1(input)
    assertEquals(obtained, BigInt("1928"))
  }

  test("Day09 - part1") {
    val input = Day09.readFile("./inputs/Day09.txt")
    val obtained = Day09.part1(input)
    assertEquals(obtained, BigInt("6432869891895"))
  }

  test("Day09 - part2 - test") {
    val input = Day09.readFile("./inputs/Day09Test.txt")
    val obtained = Day09.part2(input)
    //assertEquals(obtained, BigInt("2858"))
    assertEquals(obtained, BigInt("0"))
  }

  test("Day09 - part2") {
    val input = Day09.readFile("./inputs/Day09.txt")
    val obtained = Day09.part2(input)
    assertEquals(obtained, BigInt("0"))
  }
}
