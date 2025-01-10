package util

import util.Position.*

class GridTest extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  given Grid.GridFactory[Grid] with

    def create(
        free: Set[Position],
        blocked: Set[Position],
        start: Option[Position],
        end: Option[Position],
        dimensions: (Int, Int),
    ): Grid = new Grid(free, blocked, start, end, dimensions)

  end given

  test("Grid - source"):
    val grid = Grid.fromResource("./tests/GridTest.txt")

    assertEquals(grid.free.size, 8)
    assertEquals(grid.blocked.size, 17)
    assertEquals(grid.start.get, Position(1, 1))
    assertEquals(grid.end.get, Position(3, 3))
    assertEquals(grid.dimensions, (5, 5))

  test("Grid - adjacent"):
    val grid = Grid.fromResource("./tests/GridTest.txt")

    val obtained = grid.adjacent(Position(1, 1))
    val expected = Set(
      RPosition(1, 2, Direction.Right, Position(1, 1)),
      RPosition(2, 1, Direction.Down, Position(1, 1)),
    )
    assertEquals(obtained, expected)

  test("Grid - neighbors"):
    val grid = Grid.fromResource("./tests/GridTest.txt")

    val obtained = grid.neighbors(Position(1, 2))
    val expected = Set(
      RPosition(1, 1, Direction.Left, Position(1, 2)),
      RPosition(1, 3, Direction.Right, Position(1, 2)),
    )
    assertEquals(obtained, expected)

end GridTest
