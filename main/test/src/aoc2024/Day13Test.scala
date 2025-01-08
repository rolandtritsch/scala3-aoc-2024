package aoc2024

class Day13Test extends munit.ScalaCheckSuite:
    val only                  = new munit.Tag("only")
    val ignore                = new munit.Tag("ignore")
    override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

    import aoc2024.Day13.*

    val errorCorrection = 10000000000000L

    test("Day13 - dummy".tag(ignore)):
        assert(true)

    test("Day13 - readFile - test"):
        val obtained = Day13.readFile("./inputs/Day13Test.txt")
        // format: off
        val expected = ClawMachine(
            Button(Position(94, 34), 3),
            Button(Position(22, 67), 1),
            Position(8400, 5400)
        )
        // format: on
        assertEquals(obtained.head, expected)

    test("Day13 - readFile - test"):
        val obtained = Day13.readFile("./inputs/Day13Test.txt", errorCorrection)
        // format: off
        val expected = ClawMachine(
            Button(Position(94, 34), 3),
            Button(Position(22, 67), 1),
            Position(10000000008400L, 10000000005400L)
        )
        // format: on
        assertEquals(obtained.head, expected)

    test("Day13 - readFile"):
        val obtained = Day13.readFile("./inputs/Day13.txt")
        assertEquals(obtained.size, 320)

    test("Day13 - ordering"):
        import scala.math.Ordering.Implicits.*

        assert(Position(0, 1) > Position(0, 0))
        assert(Position(1, 0) > Position(0, 0))
        assert(Position(1, 1) > Position(0, 0))
        assert(Position(0, 0) == Position(0, 0))

    test("Day13 - chepeastWayToWin"):
        // format: off
        val machine = ClawMachine(
            Button(Position(2, 2), 3),
            Button(Position(1, 1), 1),
            Position(10, 10)
        )
        // format: on
        val obtained = machine.cheapestWayToWin(10)

        assertEquals(obtained, Some(10L))

    test("Day13 - chepeastWayToWin - test"):
        val machines = Day13.readFile("./inputs/Day13Test.txt")
        val obtained = machines.head.cheapestWayToWin(200)

        assertEquals(obtained, Some(280L))

    test("Day13 - part1 - test"):
        val input    = Day13.readFile("./inputs/Day13Test.txt")
        val obtained = Day13.part1(input)
        assertEquals(obtained, 480L)

    test("Day13 - part1"):
        val input    = Day13.readFile("./inputs/Day13.txt")
        val obtained = Day13.part1(input)
        assertEquals(obtained, 34393L)

    test("Day13 - solve - simple"):
        // format: off
        val machine = ClawMachine(
            Button(Position(94, 34), 3),
            Button(Position(22, 67), 1),
            Position(8400, 5400)
        )
        // format: on
        assertEquals(machine.solve, Some(80L, 40L))

    test("Day13 - solve - test"):
        val machines = Day13.readFile("./inputs/Day13Test.txt").toList
        assertEquals(machines.size, 4)
        assertEquals(machines(0).solve, Some(80L, 40L))
        assertEquals(machines(1).solve, None)
        assertEquals(machines(2).solve, Some(38L, 86L))
        assertEquals(machines(3).solve, None)

    test("Day13 - solve - test - p2".tag(only)):
        val machines = Day13.readFile("./inputs/Day13Test.txt", errorCorrection)
            .toList
        assertEquals(machines.size, 4)
        assertEquals(machines(0).solve, None)
        assertEquals(machines(1).solve, Some(118679050709L, 103199174542L))
        assertEquals(machines(2).solve, None)
        assertEquals(machines(3).solve, Some(102851800151L, 107526881786L))

    test("Day13 - part2 - test"):
        val input    = Day13.readFile("./inputs/Day13Test.txt", errorCorrection)
        val obtained = Day13.part2(input)
        assertEquals(obtained, 875318608908L)

    test("Day13 - part2"):
        val input    = Day13.readFile("./inputs/Day13.txt", errorCorrection)
        val obtained = Day13.part2(input)
        assertEquals(obtained, 83551068361379L)
end Day13Test
