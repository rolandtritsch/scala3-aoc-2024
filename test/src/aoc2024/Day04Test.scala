package aoc2024

class Day04Test extends munit.ScalaCheckSuite {
  test("Day04 - findAll") {
    import aoc2024.Day04._

    val obtained = Seq("..XMASXMAS.XMAS", "XMAS").findAll("XMAS")
    val expected = 4

    assertEquals(obtained, expected)
  }

  test("Day04 - transpose") {
    import aoc2024.Day04._

    val input = Seq("123", "ABC", "abc")
    val obtained = input.transpose0
    val expected = Seq("1Aa", "2Bb", "3Cc")

    assertEquals(obtained, expected)

    assertEquals(obtained.transpose0, input)
  }

  test("Day04 - diagonalize - tlbr") {
    import aoc2024.Day04._

    val input = Seq(
      "1234",
      "ABCD",
      "abcd",
      "+-*/",
    )
    val obtained = input.diagonalizeTLBR
    val expected = Seq(
      ((0, 0), "1Bc/"),
      ((0, 1), "2Cd"),
      ((0, 2), "3D"),
      ((0, 3), "4"),
      ((1, 0), "Ab*"),
      ((2, 0), "a-"),
      ((3, 0), "+"),
    )

    assertEquals(obtained, expected)
  }

  test("Day04 - diagonalize - trbl") {
    import aoc2024.Day04._

    val input = Seq(
      "1234",
      "ABCD",
      "abcd",
      "+-*/",
    )
    val obtained = input.diagonalizeTRBL
    val expected = Seq(
      ((0, 3), "4Cb+"),
      ((1, 3), "Dc-"),
      ((2, 3), "d*"),
      ((3, 3), "/"),
      ((0, 0), "1"),
      ((0, 1), "2A"),
      ((0, 2), "3Ba"),
    )

    assertEquals(obtained, expected)
  }

  test("Day04 - findAllPos - String") {
    import aoc2024.Day04._

    assertEquals(".MAS.MAS".findAllPos("MAS"), Seq(1,5))
  }

  test("Day04 - findAllPosTLBR - Puzzle") {
    import aoc2024.Day04._

    val input = Seq(
      "M.S",
      ".A.",
      "M.S",
    )
    val obtained = input.diagonalizeTLBR.findAllPosTLBR("MAS")
    val expected = Seq((1, 1))

    assertEquals(obtained, expected)
  }

  test("Day04 - findAllPosTRBL - Puzzle") {
    import aoc2024.Day04._

    val input = Seq(
      "M.S",
      ".A.",
      "M.S",
    )
    val obtained = input.diagonalizeTRBL.findAllPosTRBL("SAM")
    val expected = Seq((1, 1))

    assertEquals(obtained, expected)
  }

  test("Day04 - readFile") {
    val obtained = Day04.readFile("./inputs/Day04.txt")
    val expected = "XMXXMSSSMSXSXMMXSAMMXXSXMASMSSXXMAMXAMXSXMXSMAMMASXXASMMXMASXMSSXMMMXMXSXXSXMXXSAMXSXSXSAMXMSAMXMAXXXMXMAMSASXMSSXMSXSXXMAXXSSSMXMXMXMMAASXM"
    assertEquals(obtained(0), expected)
  }

  val input = Seq(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX",
  )

  test("Day04 - part1") {
    val obtained = Day04.part1(input)
    assertEquals(obtained, 18)
  }

  test("Day04 - part2") {
    val obtained = Day04.part2(input)
    assertEquals(obtained, 9)
  }
}
