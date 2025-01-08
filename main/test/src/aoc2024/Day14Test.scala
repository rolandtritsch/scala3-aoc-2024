package aoc2024

class Day14Test extends munit.ScalaCheckSuite:
    val only = new munit.Tag("only")
    val ignore = new munit.Tag("ignore")

    test("Day14 - dummy".tag(ignore)):
        assert(true)

    test("Day14 - readFile - test"):
        val obtained = Day14.readFile("./inputs/Day14Test.txt")
        assert(obtained.toString.nonEmpty)

    test("Day14 - readFile"):
        val obtained = Day14.readFile("./inputs/Day14.txt")
        assert(obtained.toString.nonEmpty)

    test("Day14 - quadrants - test"):
        val simulation = Day14.readFile("./inputs/Day14Test.txt")
        val obtained = simulation.run(100).quadrants().values.toList.sorted
        val expected = List(1, 1, 3, 4)
        assertEquals(obtained, expected)

    test("Day14 - part1 - test"):
        val input = Day14.readFile("./inputs/Day14Test.txt")
        val obtained = Day14.part1(input)
        assertEquals(obtained, 12)

    test("Day14 - part1"):
        val input = Day14.readFile("./inputs/Day14.txt")
        val obtained = Day14.part1(input)
        assertEquals(obtained, 228457125)

    test("Day14 - part2 - test"):
        val input = Day14.readFile("./inputs/Day14Test.txt")
        val obtained = Day14.part2(input)
        assertEquals(obtained, 12)

    test("Day14 - part2"):
        val input = Day14.readFile("./inputs/Day14.txt")
        val obtained = Day14.part2(input)
        assertEquals(obtained, 228457125)
end Day14Test
