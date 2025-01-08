package aoc2024

class Day08Test extends munit.ScalaCheckSuite:
    val only = new munit.Tag("only")
    val ignore = new munit.Tag("ignore")

    test("Day08 - dummy".tag(ignore)):
        assert(true)

    test("Day08 - readFile - test"):
        val obtained = Day08.readFile("./inputs/Day08Test.txt")
        assertEquals(obtained.dimensions, (12, 12))

    test("Day08 - readFile"):
        val obtained = Day08.readFile("./inputs/Day08.txt")
        assertEquals(obtained.dimensions, (50, 50))

    test("Day08 - anntenna.pairs"):
        import aoc2024.Day08.*

        // format: off
        val input = Set(
            Antenna('A', Position(0, 0)),
            Antenna('A', Position(1, 1)),
            Antenna('B', Position(0, 1)),
            Antenna('B', Position(1, 0))
        )
        // format: on
        val obtained = input.pairs
        // format: off
        val expected = Set(
            (Antenna('A', Position(0, 0)), Antenna('A', Position(1, 1))),
            (Antenna('B', Position(0, 1)), Antenna('B', Position(1, 0)))
        )

        assertEquals(obtained, expected)

    test("Day08 - anntenna.pairs - test"):
        import aoc2024.Day08.*

        val input    = Day08.readFile("./inputs/Day08Test.txt")
        val obtained = input.antennas.pairs
        // format: off
        val expected = Set(
            (Antenna('0', Position(2, 5)), Antenna('0', Position(1, 8))),
            (Antenna('0', Position(2, 5)), Antenna('0', Position(4, 4))),
            (Antenna('0', Position(3, 7)), Antenna('0', Position(1, 8))),
            (Antenna('0', Position(3, 7)), Antenna('0', Position(2, 5))),
            (Antenna('0', Position(3, 7)), Antenna('0', Position(4, 4))),
            (Antenna('0', Position(4, 4)), Antenna('0', Position(1, 8))),
            (Antenna('A', Position(5, 6)), Antenna('A', Position(8, 8))),
            (Antenna('A', Position(5, 6)), Antenna('A', Position(9, 9))),
            (Antenna('A', Position(9, 9)), Antenna('A', Position(8, 8)))
        )
        // format: on

        assertEquals(obtained, expected)

    test("Day08 - anntenna.pairs - test2"):
        import aoc2024.Day08.*

        val input = Day08.readFile("./inputs/Day08Test2.txt")
        val obtained = input.antennas.pairs
        // format: off
        val expected = Set(
            (Antenna('T', Position(0, 0)), Antenna('T', Position(1, 3))),
            (Antenna('T', Position(0, 0)), Antenna('T', Position(2, 1))),
            (Antenna('T', Position(1, 3)), Antenna('T', Position(2, 1)))
        )
        // format: on

        assertEquals(obtained, expected)

    // test("Day08 - positioning") {
    //   import aoc2024.Day08._
    //   import aoc2024.Day08.Positioning._

    //   val a1 = Antenna('A', Position(0, 0))
    //   val a2 = Antenna('A', Position(1, 1))

    //   assertEquals(a1.positioning(a2), UPPER_LEFT)
    //   assertEquals(a2.positioning(a1), LOWER_RIGHT)

    //   val b1 = Antenna('B', Position(0, 1))
    //   val b2 = Antenna('B', Position(1, 0))

    //   assertEquals(b1.positioning(b2), UPPER_RIGHT)
    //   assertEquals(b2.positioning(b1), LOWER_LEFT)
    // }

    test("Day08 - antiNode"):
        import aoc2024.Day08.*

        val a1 = Antenna('A', Position(0, 0))
        val a2 = Antenna('A', Position(1, 1))

        assertEquals(a1.antiNode(a2), Position(-1, -1))
        assertEquals(a2.antiNode(a1), Position(2, 2))

    test("Day08 - antiNodes"):
        import aoc2024.Day08.*

        val input = (Antenna('A', Position(0, 0)), Antenna('A', Position(1, 1)))
        val obtained = input.antiNodes
        val expected = Set(Position(-1, -1), Position(2, 2))

        assertEquals(obtained, expected)

    test("Day08 - antiNodes0"):
        import aoc2024.Day08.*

        val input = (Antenna('A', Position(4, 4)), Antenna('A', Position(5, 5)))
        val obtained = input.antiNodes0((10, 10))
        // format: off
        val expected = Set(
            Position(0, 0),
            Position(1, 1),
            Position(2, 2),
            Position(3, 3),
            Position(6, 6),
            Position(7, 7),
            Position(8, 8),
            Position(9, 9)
        )
        // format: on

        assertEquals(obtained, expected)

    test("Day08 - part1 - test"):
        val input = Day08.readFile("./inputs/Day08Test.txt")
        val obtained = Day08.part1(input)
        assertEquals(obtained, 14)

    test("Day08 - part1"):
        val input = Day08.readFile("./inputs/Day08.txt")
        val obtained = Day08.part1(input)
        assertEquals(obtained, 228)

    test("Day08 - part2 - test"):
        val input = Day08.readFile("./inputs/Day08Test.txt")
        val obtained = Day08.part2(input)
        assertEquals(obtained, 34)

    test("Day08 - part2 - test2"):
        val input = Day08.readFile("./inputs/Day08Test2.txt")
        val obtained = Day08.part2(input)
        assertEquals(obtained, 9)

    test("Day08 - part2"):
        val input = Day08.readFile("./inputs/Day08.txt")
        val obtained = Day08.part2(input)
        assertEquals(obtained, 766)
end Day08Test
