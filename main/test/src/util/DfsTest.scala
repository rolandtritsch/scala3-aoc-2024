package util

class DfsTest extends munit.ScalaCheckSuite:
  val only                  = new munit.Tag("only")
  val ignore                = new munit.Tag("ignore")
  val slow                  = new munit.Tag("slow")
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  class GridWithDfs(
    free: Set[Position],
    blocked: Set[Position],
    start: Option[Position],
    end: Option[Position],
    dimensions: (Int, Int),
  ) extends Grid(free, blocked, start, end, dimensions) with Dfs:
    def this() = this(Set.empty, Set.empty, None, None, (0, 0))

  given Grid.GridFactory[GridWithDfs] with

    def create(
      free: Set[Position],
      blocked: Set[Position],
      start: Option[Position],
      end: Option[Position],
      dimensions: (Int, Int),
    ): GridWithDfs = new GridWithDfs(free, blocked, start, end, dimensions)

  test("Dfs - findFirst"):
    val grid     = Grid.fromResource[GridWithDfs]("./tests/GridTest.txt")
    val path     = grid.findFirst(grid.start.get)
    val expected = List(
      Position(1, 1),
      Position(2, 1),
      Position(3, 1),
      Position(3, 2),
      Position(3, 3),
    )

    assertEquals(path.get, expected)

  test("Dfs - findFirst - no-path"):
    val grid = Grid.fromResource[GridWithDfs]("./tests/GridTest-NoPath.txt")
    val path = grid.findFirst(grid.start.get)

    assert(path.isEmpty)

  test("Dfs - findFirst - no-boundaries"):
    val grid     = Grid
      .fromResource[GridWithDfs]("./tests/GridTest-NoBoundaries.txt")
    val path     = grid.findFirst(grid.start.get)
    val expected = List(
      Position(0, 0),
      Position(1, 0),
      Position(2, 0),
      Position(3, 0),
      Position(3, 1),
      Position(2, 1),
      Position(1, 1),
      Position(0, 1),
      Position(0, 2),
      Position(1, 2),
      Position(1, 3),
      Position(2, 3),
      Position(3, 3),
      Position(4, 3),
      Position(4, 4),
    )

    assertEquals(path.get, expected)

  test("Dfs - findFirst - small"):
    val grid = Grid.fromResource[GridWithDfs]("./tests/GridTest-Small.txt")
    val path = grid.findFirst(grid.start.get)

    assertEquals(path.get.size, 23)

  test("Dfs - findCheapest - small"):
    val grid = Grid.fromResource[GridWithDfs]("./tests/GridTest-Small.txt")
    val path = grid.findCheapest(grid.start.get)

    assertEquals(path.get.size, 15)

  test("Dfs - findCheapest - smallMedium"):
    val grid = Grid.fromResource[GridWithDfs]("./tests/GridTest-SmallMedium.txt")
    val path = grid.findCheapest(grid.start.get)

    assertEquals(path.get.size, 25)
