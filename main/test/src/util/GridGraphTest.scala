package util

class GridGraphTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  test("GridGraph - source") {    
    val input = scala.io.Source.fromResource("./tests/GridTest.txt")
    val grid = input.grid
    val graph = GridGraph.fromGrid(grid)

    assert(!graph.isAcyclic) // The grid has cycles as you can move back and forth
    assert(!graph.isComplete) // Not every node is connected to every other node
    assert(graph.isConnected) // All traversable positions can reach each other
    assert(graph.isCyclic) // You can move back and forth between positions
    assert(!graph.isDirected) // Edges are undirected (can move both ways)
    assert(!graph.isEmpty) // The graph has nodes and edges
    assert(!graph.isHyper) // No hyperedges (edges connecting more than 2 nodes)
    assert(!graph.isMixed) // All edges are undirected
    assert(!graph.isMulti) // No multiple edges between same nodes
    assert(!graph.isTrivial) // More than one node
    assertEquals(graph.nodes.size, 8) // 8 traversable positions (excluding walls)
    assertEquals(graph.edges.size, 8) // 8 possible moves between positions (undirected)
  }

  test("GridGraph - shortestPath - small") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val grid = input.grid
    val graph = GridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end)).get.nodes
    assertEquals(path.size, 15)
  }

  test("GridGraph - shortestPath - smallMedium") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val grid = input.grid
    val graph = GridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end)).get.nodes
    assertEquals(path.size, 25)
  }

  test("GridGraph - shortestPath - medium") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Medium.txt")
    val grid = input.grid
    val graph = GridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end)).get.nodes
    assertEquals(path.size, 95)
  }

  test("GridGraph - shortestPath - large") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Large.txt")
    val grid = input.grid
    val graph = GridGraph.fromGrid(grid)
    val path = graph.get(Position(1, 1)).shortestPathTo(graph.get(grid.end)).get.nodes
    assertEquals(path.size, 195)
  }
}
