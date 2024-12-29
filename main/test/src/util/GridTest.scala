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

    assertEquals(grid.free.size, 8)
    assertEquals(grid.blocked.size, 17)
    assertEquals(grid.start.get, Position(1, 1))
    assertEquals(grid.end.get, Position(3, 3))
    assertEquals(grid.dimensions, (5, 5))
  }

  test("Grid - adjacent") {
    val grid = Grid.fromResource("./tests/GridTest.txt")

    assertEquals(grid.adjacent(Position(1, 1)), Set(Position(1, 2), Position(2, 1)))
  }

  test("Grid - neighbors") {
    val grid = Grid.fromResource("./tests/GridTest.txt")
    
    assertEquals(grid.neighbors(Position(1, 1)), Set(Position(1, 2), Position(2, 1)))
    assertEquals(grid.neighbors(Position(1, 2)), Set(Position(1, 1), Position(1, 3)))
  }
}
