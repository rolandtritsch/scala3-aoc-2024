package util

import util.Position.*

class WDGridGraphTest extends munit.ScalaCheckSuite:
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  import util.WDGridGraph.Implicits.given

  given Grid.GridFactory[Grid] with

    def create(
        free: Set[Position],
        blocked: Set[Position],
        start: Option[Position],
        end: Option[Position],
        dimensions: (Int, Int),
    ): Grid = new Grid(free, blocked, start, end, dimensions)

  end given

  test("WDGridGraph - source"):
    val grid = Grid.fromResource("./tests/GridTest.txt")
    val graph = WDGridGraph.fromGrid(grid)

    assert(!graph.isAcyclic)
    assert(!graph.isComplete)
    assert(graph.isConnected)
    assert(graph.isCyclic)
    assert(graph.isDirected)
    assert(!graph.isEmpty)
    assert(!graph.isHyper)
    assert(graph.isMixed)
    assert(!graph.isMulti)
    assert(!graph.isTrivial)
    assertEquals(graph.nodes.size, 8)
    assertEquals(graph.edges.size, 16)

  test("WDGridGraph - shortestPath - small"):
    val grid = Grid.fromResource("./tests/GridTest-Small.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes
    assertEquals(path.size, 15)

  test("WDGridGraph - shortestPath - smallMedium"):
    val grid = Grid.fromResource("./tests/GridTest-SmallMedium.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes
    assertEquals(path.size, 25)

  test("WDGridGraph - shortestPath - medium"):
    val grid = Grid.fromResource("./tests/GridTest-Medium.txt")
    val graph = GridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes
    assertEquals(path.size, 95)

  test("WDGridGraph - shortestPath - large"):
    val grid = Grid.fromResource("./tests/GridTest-Large.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end.get)).get.nodes
    assertEquals(path.size, 195)

end WDGridGraphTest
