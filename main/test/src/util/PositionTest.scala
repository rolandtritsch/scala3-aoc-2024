package util

class PositionTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Position - source") {    
    val input = scala.io.Source.fromResource("./tests/GridTest.txt")
    val position = input.start().get
    assertEquals(position, Position(1, 1))
  }
}
