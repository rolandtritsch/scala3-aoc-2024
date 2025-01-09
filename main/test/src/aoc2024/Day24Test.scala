package aoc2024

import aoc2024.Day24.*

class Day24Test extends munit.ScalaCheckSuite:
    val only = new munit.Tag("only")
    val ignore = new munit.Tag("ignore")

    test("Day24 - dummy".tag(ignore)):
        assert(true)

    test("Day24 - readFileInitials - test"):
        val obtained = readFileInitials("./inputs/Day24Test-Initials.txt")
        assertEquals(obtained.size, 10)

    test("Day24 - readFileStatements - test"):
        val obtained = readFileStatements("./inputs/Day24Test-Statements.txt")
        assertEquals(obtained.size, 36)

    test("Day24 - readFileInitials"):
        val obtained = readFileInitials("./inputs/Day24-Initials.txt")
        assertEquals(obtained.size, 90)

    test("Day24 - readFileStatements"):
        val obtained = readFileStatements("./inputs/Day24-Statements.txt")
        assertEquals(obtained.size, 222)

    test("Day24 - assignment - toString"):
        assertEquals(
            Assignment(LeftVariable("a"), Value(true)).toString,
            "def a = true"
        )
        assertEquals(
            Assignment(
                LeftVariable("b"),
                Expression(LeftVariable("c"), Operation.AND, RightVariable("d"))
            ).toString,
            "def b = c && d"
        )

    test("Day24 - generate - test2"):
        val initials = readFileInitials("./inputs/Day24Test2-Initials.txt")
        val statements =
            readFileStatements("./inputs/Day24Test2-Statements.txt")
        val obtained = generate(initials ++ statements)
        val expected = """
            |def x00 = true
            |def x01 = true
            |def x02 = true
            |def y00 = false
            |def y01 = true
            |def y02 = false
            |def z00 = x00 && y00
            |def z01 = x01 ^ y01
            |def z02 = x02 || y02
            |List(z02,z01,z00).map(if(_) 1 else 0).mkString
            |""".stripMargin.trim
        assertEquals(obtained, expected)

    test("Day24 - evaluate - basic"):
        val code = """
            |def d = c || e
            |def a = true
            |def b = true
            |def c = a && b
            |def e = false
            |List(a, b, c, d, e).map(if(_) 1 else 0).mkString
            |""".stripMargin.trim
        val obtained = evaluate(code)
        assertEquals(obtained, "11110")

    test("Day24 - evaluate - test2"):
        val initials = readFileInitials("./inputs/Day24Test2-Initials.txt")
        val statements =
            readFileStatements("./inputs/Day24Test2-Statements.txt")
        val code = generate(initials ++ statements)

        val obtained = evaluate(code)
        assertEquals(obtained, "100")

    test("Day24 - part1 - test"):
        val initials = readFileInitials("./inputs/Day24Test-Initials.txt")
        val statements = readFileStatements("./inputs/Day24Test-Statements.txt")
        val obtained = part1(initials, statements)
        assertEquals(obtained, BigInt(2024))

    test("Day24 - part1"):
        val initials = readFileInitials("./inputs/Day24-Initials.txt")
        val statements = readFileStatements("./inputs/Day24-Statements.txt")
        val obtained = part1(initials, statements)
        assertEquals(obtained, BigInt("56729630917616"))

    test("Day24 - part2 - test"):
        val initials = readFileInitials("./inputs/Day24Test-Initials.txt")
        val statements = readFileStatements("./inputs/Day24Test-Statements.txt")
        val obtained = part2(initials, statements)
        assertEquals(obtained, 46)

    test("Day24 - part2"):
        val initials = readFileInitials("./inputs/Day24-Initials.txt")
        val statements = readFileStatements("./inputs/Day24-Statements.txt")
        val obtained = part2(initials, statements)
        assertEquals(obtained, 312)
end Day24Test
