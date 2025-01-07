package aoc2024

import aoc2024.Day16.*

class Day16Test extends munit.ScalaCheckSuite:
    val only   = new munit.Tag("only")
    val ignore = new munit.Tag("ignore")

    test("Day16 - dummy".tag(ignore)):
        assert(true)

    test("Day16 - readFile - test"):
        val (maze, reindeer) = readFile("./inputs/Day16Test.txt")
        assertEquals(maze.exit, Position(1, 13))
        assertEquals(reindeer.position, Position(13, 1))

    test("Day16 - readFile"):
        val (maze, reindeer) = readFile("./inputs/Day16.txt")
        assertEquals(maze.exit, Position(1, 139))
        assertEquals(reindeer.position, Position(139, 1))

    test("Day16 - part1 - test"):
        val input    = readFile("./inputs/Day16Test.txt")
        val obtained = part1(input)
        assertEquals(obtained, 7036)

    test("Day16 - part1 - test2"):
        val input    = readFile("./inputs/Day16Test2.txt")
        val obtained = part1(input)
        assertEquals(obtained, 11048)

    test("Day16 - part1 - test9"):

        val input    = readFile("./inputs/Day16Test9.txt")
        val obtained = part1(input)
        assertEquals(obtained, 3030)

    // test("Day16 - part1") {
    //   val input = readFile("./inputs/Day16.txt")
    //   val obtained = part1(input)
    //   assertEquals(obtained, 0)
    // }

    // Test("Day16 - part2 - test") {
    //   val input = readFile("./inputs/Day16Test.txt")
    //   val obtained = part2(input)
    //   assertEquals(obtained, 13)
    // }

    // test("Day16 - part2") {
    //   val input = readFile("./inputs/Day16.txt")
    //   val obtained = part2(input)
    //   assertEquals(obtained, 139)
    // }
end Day16Test
