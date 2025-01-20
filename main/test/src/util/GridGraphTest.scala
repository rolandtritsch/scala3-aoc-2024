package util

import util.Position.*

class GridGraphTest extends munit.ScalaCheckSuite:
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

  test("GridGraph - source"):
    val grid = Grid.fromResource("./tests/GridTest.txt")
    val graph = GridGraph.fromGrid(grid)

    assert(!graph.isAcyclic)
    assert(!graph.isComplete)
    assert(graph.isConnected)
    assert(graph.isCyclic)
    assert(!graph.isDirected)
    assert(!graph.isHyper)
    assert(!graph.isMixed)
    assert(!graph.isMulti)
    assert(!graph.isTrivial)
    assertEquals(graph.nodes.size, 8)
    assertEquals(graph.edges.size, 8)

  test("GridGraph - shortestPath - small"):
    val grid = Grid.fromResource("./tests/GridTest-Small.txt")
    val graph = GridGraph.fromGrid(grid)
    val path =
      graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes // scalafix:ok
    assertEquals(path.size, 15)

  test("GridGraph - shortestPath - smallMedium"):
    val grid = Grid.fromResource("./tests/GridTest-SmallMedium.txt")
    val graph = GridGraph.fromGrid(grid)
    val path =
      graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes // scalafix:ok
    assertEquals(path.size, 25)

  test("GridGraph - shortestPath - medium"):
    val grid = Grid.fromResource("./tests/GridTest-Medium.txt")
    val graph = GridGraph.fromGrid(grid)
    val path =
      graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes // scalafix:ok
    assertEquals(path.size, 95)

  test("GridGraph - shortestPath - large"):
    val grid = Grid.fromResource("./tests/GridTest-Large.txt")
    val graph = GridGraph.fromGrid(grid)
    val path =
      graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes // scalafix:ok
    assertEquals(path.size, 195)

end GridGraphTest
