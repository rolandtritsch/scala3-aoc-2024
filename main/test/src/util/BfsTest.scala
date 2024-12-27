package util

class BfsTest extends munit.ScalaCheckSuite {
  val only = new munit.Tag("only")
  val ignore = new munit.Tag("ignore")
  val slow = new munit.Tag("slow")
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  class GridWithBfs(obstacles: Set[Position], end: Position) extends Grid(obstacles, end) with Bfs

  test("Bfs - findFirst") {    
    val input = scala.io.Source.fromResource("./tests/GridTest.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest.txt")
    val start = input2.start().get
    val pathIterative = new GridWithBfs(grid.obstacles, grid.end).findFirstIterative(start)
    val pathRecursive = new GridWithBfs(grid.obstacles, grid.end).findFirstRecursive(start)
    val expected = List(
      Position(1, 1),
      Position(1, 2),
      Position(1, 3),
      Position(2, 3),
      Position(3, 3),      
    )

    assertEquals(pathIterative.get, expected)
    assertEquals(pathRecursive.get, expected)
  }

  test("Bfs - findFirst - no-path") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-NoPath.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-NoPath.txt")
    val start = input2.start().get
    val pathIterative = new GridWithBfs(grid.obstacles, grid.end).findFirstIterative(start)
    val pathRecursive = new GridWithBfs(grid.obstacles, grid.end).findFirstRecursive(start)

    assert(pathIterative.isEmpty)
    assert(pathRecursive.isEmpty)
  }

  test("Bfs - findFirst - no-boundaries") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-NoBoundaries.txt")
    val grid = input.grid.surround()
    val input2 = scala.io.Source.fromResource("./tests/GridTest-NoBoundaries.txt")
    val start = input2.start().get
    val pathIterative = new GridWithBfs(grid.obstacles, grid.end).findFirstIterative(start)
    val pathRecursive = new GridWithBfs(grid.obstacles, grid.end).findFirstRecursive(start)
    val expected = List(
      Position(0, 0),
      Position(0, 1),
      Position(0, 2),
      Position(0, 3),
      Position(1, 3),
      Position(1, 4),
      Position(2, 4),
      Position(3, 4),
      Position(4, 4),
    )
    
    assertEquals(pathIterative.get, expected)
    assertEquals(pathRecursive.get, expected)
  }

  test("Bfs - findFirst - small") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-Small.txt")
    val start = input2.start().get
    val pathIterative = new GridWithBfs(grid.obstacles, grid.end).findFirstIterative(start)
    val pathRecursive = new GridWithBfs(grid.obstacles, grid.end).findFirstRecursive(start)

    assertEquals(pathIterative.get.size, 15)
    assertEquals(pathRecursive.get.size, 15)
  }

  test("Bfs - findFirst - smallMedium") {    
    val input = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-SmallMedium.txt")
    val start = input2.start().get
    val pathIterative = new GridWithBfs(grid.obstacles, grid.end).findFirstIterative(start)
    //val pathRecursive = new GridWithBfs(grid.obstacles, grid.end).findFirstRecursive(start)
    //println(grid.toStringPretty(start, path.get))
    assertEquals(pathIterative.get.size, 25)        
    //assertEquals(pathRecursive.get.size, 25)        
  }

  test("Bfs - findFirst - medium") {
    val input = scala.io.Source.fromResource("./tests/GridTest-Medium.txt")
    val grid = input.grid
    val input2 = scala.io.Source.fromResource("./tests/GridTest-Medium.txt")
    val start = input2.start().get
    val pathIterative = new GridWithBfs(grid.obstacles, grid.end).findFirstIterative(start)
    //val pathRecursive = new GridWithBfs(grid.obstacles, grid.end).findFirstRecursive(start)
    //println(grid.toStringPretty(start, path.get)) 
    assertEquals(pathIterative.get.size, 95)
    //assertEquals(pathRecursive.get.size, 95)
  }
}
