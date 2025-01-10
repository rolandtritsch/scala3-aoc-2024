package util

import util.Position.*

class BfsTest extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  val slow = new munit.Tag("slow")
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  class GridWithBfs(
      free: Set[Position],
      blocked: Set[Position],
      start: Option[Position],
      end: Option[Position],
      dimensions: (Int, Int),
  ) extends Grid(free, blocked, start, end, dimensions) with Bfs:
    def this() = this(Set.empty, Set.empty, None, None, (0, 0))
  end GridWithBfs

  given Grid.GridFactory[GridWithBfs] with

    def create(
        free: Set[Position],
        blocked: Set[Position],
        start: Option[Position],
        end: Option[Position],
        dimensions: (Int, Int),
    ): GridWithBfs = new GridWithBfs(free, blocked, start, end, dimensions)

  end given

  test("Bfs - findFirst"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest.txt")
    val pathIterative = grid.findFirstIterative()
    val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)), Set(grid.start.get))
    val expected =
      List(Position(1, 1), Position(2, 1), Position(3, 1), Position(3, 2), Position(3, 3))

    assertEquals(pathIterative.get, expected)
    assertEquals(pathRecursive.get, expected)

  test("Bfs - findFirst - no-path"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest-NoPath.txt")
    val pathIterative = grid.findFirstIterative()
    val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)), Set(grid.start.get))

    assert(pathIterative.isEmpty)
    assert(pathRecursive.isEmpty)

  test("Bfs - findFirst - no-boundaries"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest-NoBoundaries.txt")
    val pathIterative = grid.findFirstIterative()
    val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)), Set(grid.start.get))
    val expectedIterative = List(
      Position(0, 0),
      Position(1, 0),
      Position(2, 0),
      Position(3, 0),
      Position(3, 1),
      Position(4, 1),
      Position(4, 2),
      Position(4, 3),
      Position(4, 4),
    )
    val expectedRecursive = List(
      Position(0, 0),
      Position(1, 0),
      Position(2, 0),
      Position(2, 1),
      Position(3, 1),
      Position(3, 2),
      Position(4, 2),
      Position(4, 3),
      Position(4, 4),
    )

    assertEquals(pathIterative.get, expectedIterative)
    assertEquals(pathRecursive.get, expectedRecursive)

  test("Bfs - findFirst - small"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest-Small.txt")
    val pathIterative = grid.findFirstIterative()
    val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)), Set(grid.start.get))

    assertEquals(pathIterative.get.size, 15)
    assertEquals(pathRecursive.get.size, 15)

  test("Bfs - findFirst - smallMedium"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest-SmallMedium.txt")
    val pathIterative = grid.findFirstIterative()
    val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)), Set(grid.start.get))

    assertEquals(pathIterative.get.size, 25)
    assertEquals(pathRecursive.get.size, 25)

  test("Bfs - findFirst - medium"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest-Medium.txt")
    val pathIterative = grid.findFirstIterative()
    /* val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)),
     * Set(grid.start.get)) */
    // println(grid.toStringPretty(start, path.get))
    assertEquals(pathIterative.get.size, 95)
    // assertEquals(pathRecursive.get.size, 95)

  test("Bfs - findFirst - large"):
    val grid = Grid.fromResource[GridWithBfs]("./tests/GridTest-Large.txt")
    val pathIterative = grid.findFirstIterative()
    /* val pathRecursive = grid.findFirstRecursive(Set(List(grid.start.get)),
     * Set(grid.start.get)) */
    // println(grid.toStringPretty(start, path.get))
    assertEquals(pathIterative.get.size, 195)
    // assertEquals(pathRecursive.get.size, 195)

end BfsTest
