package util

class GridTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("Grid - source") {    
    val input = scala.io.Source.fromResource("./tests/GridTest.txt")
    val grid = input.grid
    assertEquals(grid.end, Position(3, 3))
  }
}
