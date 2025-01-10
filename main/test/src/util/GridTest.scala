package util

class GridTest extends munit.ScalaCheckSuite:
  import util.DPosition.*
  import util.Grid.Factory.given

  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

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
      DPosition(1, 2, Direction.Right),
      DPosition(2, 1, Direction.Down),
    )
    assertEquals(obtained, expected)

  test("Grid - neighbors"):
    val grid = Grid.fromResource("./tests/GridTest.txt")

    val obtained = grid.neighbors(Position(1, 2))
    val expected = Set(
      DPosition(1, 1, Direction.Left),
      DPosition(1, 3, Direction.Right),
    )
    assertEquals(obtained, expected)

end GridTest
