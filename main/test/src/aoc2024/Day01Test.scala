package aoc2024

class Day01Test extends munit.ScalaCheckSuite:

    test("Day01 - readFile"):
        val obtained = Day01.readFile("./inputs/Day01.txt")
        assertEquals(obtained(0), (64430, 75582))

    val input = Seq((3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3))

    test("Day01 - part1 - test"):
        val obtained = Day01.part1(input)
        assertEquals(obtained, 11)

    test("Day01 - part1"):
        val input = Day01.readFile("./inputs/Day01.txt")
        val obtained = Day01.part1(input)
        assertEquals(obtained, 2057374)

    test("Day01 - part2 - test"):
        val obtained = Day01.part2(input)
        assertEquals(obtained, 31)

    test("Day01 - part2"):
        val input = Day01.readFile("./inputs/Day01.txt")
        val obtained = Day01.part2(input)
        assertEquals(obtained, 23177084)
end Day01Test
