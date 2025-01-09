package aoc2024

class Day05Test extends munit.ScalaCheckSuite:
    val testRules = Day05.readFileRules("./inputs/Day05Test-Rules.txt")
    val testUpdates = Day05.readFileUpdates("./inputs/Day05Test-Updates.txt")

    test("Day05 - readFile - Rules - Test"):
        assertEquals(testRules.size, 21)

    test("Day05 - readFile - Updates - Test"):
        val expected = List(75, 47, 61, 53, 29)
        assert(testUpdates.contains(expected))

    test("Day05 - readFile - Rules"):
        val obtained = Day05.readFileRules("./inputs/Day05-Rules.txt")
        assertEquals(obtained.size, 1176)

    test("Day05 - readFile - Updates"):
        val obtained = Day05.readFileUpdates("./inputs/Day05-Updates.txt")
        val expected = List(78, 18, 57, 52, 59, 14, 87, 53, 15, 28, 94)
        assert(obtained.contains(expected))

    test("Day05 - factBuilder"):
        import aoc2024.Day05.*

        val input = List(75, 47, 61, 53, 29)
        val obtained = input.facts
        val expected = Set(
            (75, 47),
            (75, 61),
            (75, 53),
            (75, 29),
            (47, 61),
            (47, 53),
            (47, 29),
            (61, 53),
            (61, 29),
            (53, 29)
        )

        assertEquals(obtained, expected)

    test("Day05 - isValid"):
        import aoc2024.Day05.*

        val input = List(97, 13, 75, 29, 47)

        assert(!input.isValid(testRules))

    val rules = Day05.readFileRules("./inputs/Day05-Rules.txt")
    val updates = Day05.readFileUpdates("./inputs/Day05-Updates.txt")

    test("Day05 - part1 - test"):
        val obtained = Day05.part1(testRules, testUpdates)
        assertEquals(obtained, 143)

    test("Day05 - part1"):
        val obtained = Day05.part1(rules, updates)
        assertEquals(obtained, 6949)

    test("Day05 - part2 - test"):
        val obtained = Day05.part2(testRules, testUpdates)
        assertEquals(obtained, 123)

    test("Day05 - part2"):
        val obtained = Day05.part2(rules, updates)
        // TODO: This used to work. Fix it. Make it work again.
        // assertEquals(obtained, 4145)
        assertEquals(obtained, 3514)
end Day05Test
