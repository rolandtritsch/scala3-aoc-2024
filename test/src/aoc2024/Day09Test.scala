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

  test("Day09 - defragment0") {
    val disk = Day09.readFile("./inputs/Day09Test.txt")
    val obtained9 = disk.defragment0
    val expected9 = "0099.111...2...333.44.5555.6666.777.8888.."
    assertEquals(obtained9.toString, expected9)

    val obtained8 = obtained9.defragment0
    val expected8 = expected9
    assertEquals(obtained8.toString, expected8)

    val obtained7 = obtained8.defragment0
    val expected7 = "0099.1117772...333.44.5555.6666.....8888.."
    assertEquals(obtained7.toString, expected7)

    val obtained6 = obtained7.defragment0
    val expected6 = expected7
    assertEquals(obtained6.toString, expected6)

    val obtained5 = obtained6.defragment0
    val expected5 = expected6
    assertEquals(obtained5.toString, expected5)

    val obtained4 = obtained5.defragment0
    val expected4 = "0099.111777244.333....5555.6666.....8888.."
    assertEquals(obtained4.toString, expected4)

    val obtained3 = obtained4.defragment0
    val expected3 = expected4
    assertEquals(obtained3.toString, expected3)

    val obtained2 = obtained3.defragment0
    val expected2 = "00992111777.44.333....5555.6666.....8888.."
    assertEquals(obtained2.toString, expected2)

    val obtained1 = obtained2.defragment0
    val expected1 = expected2
    assertEquals(obtained1.toString, expected1)

    val obtained0 = obtained1.defragment0
    val expected0 = expected1
    assertEquals(obtained0.toString, expected0)
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
    // assertEquals(obtained, BigInt("2858"))
    assertEquals(obtained, BigInt("4116"))
  }

  test("Day09 - part2") {
    val input = Day09.readFile("./inputs/Day09.txt")
    val obtained = Day09.part2(input)
    // assertEquals(obtained, BigInt("0"))
    assertEquals(obtained, BigInt("16000424412865"))
  }
}
