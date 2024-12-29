package util

class GridTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  given Grid.GridFactory[Grid] with {
    def create(free: Set[Position], blocked: Set[Position], start: Option[Position], end: Option[Position], dimensions: (Int, Int)): Grid =
      new Grid(free, blocked, start, end, dimensions)
  }

  test("Grid - source") {
    val grid = Grid.fromResource("./tests/GridTest.txt")
    assertEquals(grid.end.get, Position(3, 3))
  }
}
