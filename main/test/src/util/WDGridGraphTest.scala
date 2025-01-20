package util

class WDGridGraphTest extends munit.ScalaCheckSuite:

  import util.Grid.Factory.given
  import util.WDGridGraph.Implicits.given
  import util.DPosition.*

  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("WDGridGraph - source"):
    val grid = Grid.fromResource("./tests/GridTest.txt")
    val graph = WDGridGraph.fromGrid(grid)

    assert(!graph.isAcyclic)
    assert(!graph.isComplete)
    assert(graph.isConnected)
    assert(graph.isCyclic)
    assert(graph.isDirected)
    assert(!graph.isHyper)
    assert(graph.isMixed)
    assert(!graph.isMulti)
    assert(!graph.isTrivial)
    assertEquals(graph.nodes.size, 32)
    assertEquals(graph.edges.size, 48)

  test("WDGridGraph - shortestPath - small"):
    val grid = Grid.fromResource("./tests/GridTest-Small.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val startPos = DPosition(1, 1, Direction.Right)
    val endPos = DPosition(grid.end.get.x, grid.end.get.y, Direction.Down)
    val path = graph.get(startPos).shortestPathTo(graph.get(endPos)).get.nodes // scalafix:ok
    assertEquals(path.size, 15)

  test("WDGridGraph - shortestPath - smallMedium"):
    val grid = Grid.fromResource("./tests/GridTest-SmallMedium.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val startPos = DPosition(1, 1, Direction.Right)
    val endPos = DPosition(grid.end.get.x, grid.end.get.y, Direction.Down)
    val path = graph.get(startPos).shortestPathTo(graph.get(endPos)).get.nodes // scalafix:ok
    assertEquals(path.size, 25)

  test("WDGridGraph - shortestPath - medium"):
    val grid = Grid.fromResource("./tests/GridTest-Medium.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val startPos = DPosition(1, 1, Direction.Right)
    val endPos = DPosition(grid.end.get.x, grid.end.get.y, Direction.Down)
    val path = graph.get(startPos).shortestPathTo(graph.get(endPos)).get.nodes // scalafix:ok
    assertEquals(path.size, 95)

  test("WDGridGraph - shortestPath - large"):
    val grid = Grid.fromResource("./tests/GridTest-Large.txt")
    val graph = WDGridGraph.fromGrid(grid)
    val startPos = DPosition(1, 1, Direction.Right)
    val endPos = DPosition(grid.end.get.x, grid.end.get.y, Direction.Down)
    val path = graph.get(startPos).shortestPathTo(graph.get(endPos)).get.nodes // scalafix:ok
    assertEquals(path.size, 195)

end WDGridGraphTest
