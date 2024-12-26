package util

class DfsTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")

  class GridWithDfs(obstacles: Set[Position], end: Position) extends Grid(obstacles, end) with Dfs

  test("Dfs - findFirst") {    
    val input = scala.io.Source.fromResource("./tests/GridTest.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)
    val expected = List(
      Position(1, 1),
      Position(2, 1),
      Position(3, 1),
      Position(3, 2),
      Position(3, 3),      
    )

    assertEquals(path.get, expected)
  }

  test("Dfs - findFirst - no-path") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-NoPath.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-NoPath.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)

    assert(path.isEmpty)
  }

  test("Dfs - findFirst - no-boundaries") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-NoBoundaries.txt")
    val grid = input.grid.surround()
    val input2 = scala.io.Source.fromResource("./tests/GridTest-NoBoundaries.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)
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
  }

  test("Dfs - findFirst - large") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Large.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-Large.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)
        
    assertEquals(path.get.size, 4707)
  }
}
