package aoc2024

class Day02Test extends munit.ScalaCheckSuite:

    test("Day02 - readFile"):
        val obtained = Day02.readFile("./inputs/Day02.txt")
        assertEquals(obtained(0), Seq(16, 17, 18, 21, 23, 24, 27, 24))

    val input = Seq(
      Seq(7, 6, 4, 2, 1),
      Seq(1, 2, 7, 8, 9),
      Seq(9, 7, 6, 2, 1),
      Seq(1, 3, 2, 4, 5),
      Seq(8, 6, 4, 4, 1),
      Seq(1, 3, 6, 7, 9)
    )

    test("Day02 - part1 - test"):
        val obtained = Day02.part1(input)
        assertEquals(obtained, 2)

    test("Day02 - part1"):
        val input    = Day02.readFile("./inputs/Day02.txt")
        val obtained = Day02.part1(input)
        assertEquals(obtained, 585)

    test("Day02 - part2 - test"):
        val obtained = Day02.part2(input)
        assertEquals(obtained, 4)

    test("Day02 - part2"):
        val input    = Day02.readFile("./inputs/Day02.txt")
        val obtained = Day02.part2(input)
        assertEquals(obtained, 626)
end Day02Test
