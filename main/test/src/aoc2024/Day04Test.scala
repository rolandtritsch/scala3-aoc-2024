package aoc2024

class Day04Test extends munit.ScalaCheckSuite:

    test("Day04 - findAll"):
        import aoc2024.Day04.*

        val obtained = Seq("..XMASXMAS.XMAS", "XMAS").findAll("XMAS")
        val expected = 4

        assertEquals(obtained, expected)

    test("Day04 - rotate"):
        import aoc2024.Day04.*

        val input    = Seq("123", "ABC", "abc")
        val obtained = input.rotate
        val expected = Seq("aA1", "bB2", "cC3")

        assertEquals(obtained, expected)

        assertEquals(obtained.rotate.rotate.rotate, input)

    test("Day04 - transpose"):
        import aoc2024.Day04.*

        val input    = Seq("123", "ABC", "abc")
        val obtained = input.transposed
        val expected = Seq("1Aa", "2Bb", "3Cc")

        assertEquals(obtained, expected)

        assertEquals(obtained.transposed, input)

    test("Day04 - diagonalize - tlbr"):
        import aoc2024.Day04.*

        val input    = Seq("1234", "ABCD", "abcd", "+-*/")
        val obtained = input.diagonalizeTLBR
        val expected = Seq(
          ((0, 0), "1Bc/"),
          ((0, 1), "2Cd"),
          ((0, 2), "3D"),
          ((0, 3), "4"),
          ((1, 0), "Ab*"),
          ((2, 0), "a-"),
          ((3, 0), "+")
        )

        assertEquals(obtained, expected)

    test("Day04 - diagonalize - trbl"):
        import aoc2024.Day04.*

        val input    = Seq("1234", "ABCD", "abcd", "+-*/")
        val obtained = input.diagonalizeTRBL
        val expected = Seq(
          ((0, 3), "4Cb+"),
          ((1, 3), "Dc-"),
          ((2, 3), "d*"),
          ((3, 3), "/"),
          ((0, 0), "1"),
          ((0, 1), "2A"),
          ((0, 2), "3Ba")
        )

        assertEquals(obtained, expected)

    test("Day04 - findAllPos - String"):
        import aoc2024.Day04.*

        assertEquals(".MAS.MAS".findAllPos("MAS"), Seq(1, 5))

    test("Day04 - findAllPos - TLBR - Puzzle"):
        import aoc2024.Day04.*

        val input = Seq("M.S", ".A.", "M.S")
        val obtained = input.diagonalizeTLBR
            .findAllPos("MAS", Position.nextTLBR)
        val expected = Seq((0, 0))

        assertEquals(obtained, expected)

    test("Day04 - findAllPos - TRBL - Puzzle"):
        import aoc2024.Day04.*

        val input = Seq("M.S", ".A.", "M.S")
        val obtained = input.diagonalizeTRBL
            .findAllPos("SAM", Position.nextTRBL)
        val expected = Seq((0, 2))

        assertEquals(obtained, expected)

    test("Day04 - readFile"):
        val obtained = Day04.readFile("./inputs/Day04.txt")
        val expected =
            "XMXXMSSSMSXSXMMXSAMMXXSXMASMSSXXMAMXAMXSXMXSMAMMASXXASMMXMASXMSSXMMMXMXSXXSXMXXSAMXSXSXSAMXMSAMXMAXXXMXMAMSASXMSSXMSXSXXMAXXSSSMXMXMXMMAASXM"
        assertEquals(obtained(0), expected)

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
      "MXMXAXMASX"
    )

    test("Day04 - part1 - test"):
        val obtained = Day04.part1(input)
        assertEquals(obtained, 18)

    test("Day04 - part1"):
        val input    = Day04.readFile("./inputs/Day04.txt")
        val obtained = Day04.part1(input)
        assertEquals(obtained, 2536)

    test("Day04 - part2 - test"):
        val obtained = Day04.part2(input)
        assertEquals(obtained, 9)

    test("Day04 - part2"):
        val input    = Day04.readFile("./inputs/Day04.txt")
        val obtained = Day04.part2(input)
        assertEquals(obtained, 1875)
end Day04Test
