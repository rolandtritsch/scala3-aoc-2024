package util

class DfsTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  val slow = new munit.Tag("slow")
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  class GridWithDfs(obstacles: Set[Position], end: Position) extends Grid(obstacles, end) with Dfs

  test("Dfs - findFirst") {    
    val input = scala.io.Source.fromResource("./tests/GridTest.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)
    val expected = List(
      Position(1, 1),
      Position(1, 2),
      Position(1, 3),
      Position(2, 3),
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
      Position(0, 1),
      Position(0, 2),
      Position(0, 3),
      Position(1, 3),
      Position(1, 2),
      Position(1, 1),
      Position(1, 0),
      Position(2, 0),
      Position(2, 1),
      Position(3, 1),
      Position(3, 2),
      Position(3, 3),
      Position(2, 3),
      Position(2, 4),
      Position(3, 4),
      Position(4, 4),
    )
    
    assertEquals(path.get, expected)
  }

  test("Dfs - findFirst - small") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)

    assertEquals(path.get.size, 23)
  }

  test("Dfs - findFirst - smallMedium") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findFirst(start)
    //println(grid.toStringPretty(start, path.get))
    assertEquals(path.get.size, 53)        
  }

  test("Dfs - findCheapest - small") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findCheapest(start)
        
    assertEquals(path.get.size, 15)
  }

  test("Dfs - findCheapest - smallMedium") {
    val input = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findCheapest(start)
    // println(grid.toStringPretty(start, path.get)) 
    assertEquals(path.get.size, 25)
  }

  test("Dfs - findCheapest - medium") {
    val input = scala.io.Source.fromResource("./tests/GridTest-Medium.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-Medium.txt")
    val start = input2.start().get
    val path = new GridWithDfs(grid.obstacles, grid.end).findCheapest(start)
    //println(grid.toStringPretty(start, path.get)) 
    assertEquals(path.get.size, 95)
  }
}
