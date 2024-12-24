package aoc2024

import aoc2024.Day24._

class Day24Test extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Day24 - dummy".tag(ignore)) {
    assert(true)
  }

  test("Day24 - readFileInitials - test") {
    val obtained = readFileInitials("./inputs/Day24Test-Initials.txt")
    assertEquals(obtained.head, Assignment(LeftVariable("x01"), Value(false)))
  }

  test("Day24 - readFileStatements - test") {
    val obtained = readFileStatements("./inputs/Day24Test-Statements.txt")
    assertEquals(obtained.size, 36)
  }

  test("Day24 - readFileInitials") {
    val obtained = readFileInitials("./inputs/Day24-Initials.txt")
    assertEquals(obtained.size, 90)
  }

  test("Day24 - readFileStatements") {
    val obtained = readFileStatements("./inputs/Day24-Statements.txt")
    assertEquals(obtained.size, 222)
  }

  test("Day24 - part1 - test") {
    val initials = readFileInitials("./inputs/Day24Test-Initials.txt")
    val statements = readFileStatements("./inputs/Day24Test-Statements.txt")
    val obtained = part1(initials, statements)
    assertEquals(obtained, 46)
  }

  test("Day24 - part1") {
    val initials = readFileInitials("./inputs/Day24-Initials.txt")
    val statements = readFileStatements("./inputs/Day24-Statements.txt")
    val obtained = part1(initials, statements)
    assertEquals(obtained, 312)
  }

  test("Day24 - part2 - test") {
    val initials = readFileInitials("./inputs/Day24Test-Initials.txt")
    val statements = readFileStatements("./inputs/Day24Test-Statements.txt")
    val obtained = part2(initials, statements)
    assertEquals(obtained, 46)
  }

  test("Day24 - part2") {
    val initials = readFileInitials("./inputs/Day24-Initials.txt")
    val statements = readFileStatements("./inputs/Day24-Statements.txt")
    val obtained = part2(initials, statements)
    assertEquals(obtained, 312)
  }
}
