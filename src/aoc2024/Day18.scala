package aoc2024

/** Day18 - RAM Run
  *
  * Sounds like another dfs grid traversal problem to find the
  * shortest path from the start to the end.
  * 
  * We need a Position.
  * 
  * We then read in the corrupted memory positions. Note: To make
  * visualizing the memory space easier, we need to switch the X
  * and Y coordinates.
  * 
  * To make the grid traversal easier, we will surround the memory
  * with corrupted memory (based on some given dimensions). That
  * way we never need to check for the boundaries of the grid. We
  * just check for corrupted memory. 
  */

object Day18 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  case class Position(x: Int, y: Int) {
    def next(corruptedMemory: Set[Position], visited: Map[Position, Int], path: List[Position]): Seq[Position] = {
      val potentialNeighbors = Seq(
        Position(x - 1, y),
        Position(x + 1, y),
        Position(x, y - 1),
        Position(x, y + 1)
      )

      potentialNeighbors.filter { n => {
        !corruptedMemory.contains(n) && (!visited.contains(n) || visited(n) > path.size + 1)
      }}
    }

    def findShortestPath(end: Position, memory: Set[Position], visited: Map[Position, Int] = Map.empty, path: List[Position] = List.empty, shortestPath: Option[List[Position]] = None): Option[List[Position]] = {
      if (this == end) 
        if (shortestPath.isEmpty || path.size < shortestPath.get.size) Some(path)
        else shortestPath 
      else next(memory, visited, path).foldLeft(shortestPath) { (sp, n) => {
        // val v = visited + (this -> math.min(visited.getOrElse(this, Int.MaxValue), path.size))
        val v = visited + (this -> path.size)
        val p = path :+ this
        val nsp = n.findShortestPath(end, memory, v, p)
        if (nsp.isEmpty) sp else nsp
      }}
    }
  }

  extension (memory: Set[Position]) {
    def surround(dimensions: (Int, Int)): Set[Position] = {
      val (maxX, maxY) = dimensions
      val left = (0 until maxX).map { x => Position(x, -1) }
      val right = (0 until maxX).map { x => Position(x, maxY) }
      val top = (0 until maxY).map { y => Position(-1, y) }
      val bottom = (0 until maxY).map { y => Position(maxX, y) }

      memory ++ left ++ right ++ top ++ bottom
    }
  }

  /** @return Set of corrupted memory positions from the given file */
  def readFile(filename: String, numberOfLines: Int = Int.MaxValue): Set[Position] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.take(numberOfLines).map { line =>
        logger.debug(s"line: ${line}")
        val parsed = line.split(",").map(_.toInt)
        logger.debug(s"parsed: ${parsed}")
        assert(parsed.size == 2, s"parsed.size == 2: ${parsed.size}")
        Position(parsed(1), parsed(0))
      }.toSet
    } finally {
      source.close()
    }
  }

  /** @return the shortest path through the corrupted memory */
  def part1(corruptedMemory: Set[Position], dimensions: (Int, Int)): Int = {
    require(corruptedMemory.nonEmpty, "corruptedMemory.nonEmpty")
    logger.debug(s"corruptedMemory: ${corruptedMemory}")

    val (maxX, maxY) = dimensions

    val start = Position(0, 0)
    val end = Position(maxX - 1, maxY - 1)

    val memory = corruptedMemory.surround(dimensions)
    val shortestPath = start.findShortestPath(end, memory).getOrElse(List.empty)
    println(shortestPath)
    shortestPath.size
  }

  /** @return the solution for part2 */
  def part2(corruptedMemory: Set[Position], dimensions: (Int, Int)): Int = {
    require(corruptedMemory.nonEmpty, "corruptedMemory.nonEmpty")
    logger.debug(s"corruptedMemory: ${corruptedMemory}")

    corruptedMemory.size
  }
}
