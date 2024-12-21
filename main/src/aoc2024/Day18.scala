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
  * 
  * As always we need keep track of the visited positions (to prevent
  * loops) and the shortest path for the visited positions (to prune/
  * accelerate the search) and the shortest path found so far.
  * 
  * Note: Depending on the size of the memory we might need to
  * change the number of lines read in (numberOfLines).
  * 
  * We probably also need to adjust the stack size (to make sure
  * we do not run out of stack). And might need to adjust the
  * the maximum time allowed to run the tests.
  *  
  * Part1:
  * 
  * - Read the file/memory
  * - Do a recursive depth-first search (dfs)
  * - Return the shortest path found
  */

object Day18 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  val visited = scala.collection.mutable.Map.empty[Position, Int].withDefaultValue(Int.MaxValue)

  case class Position(x: Int, y: Int) {
    def next(corruptedMemory: Set[Position]): Seq[Position] = {
      Seq(
        Position(x - 1, y),
        Position(x + 1, y),
        Position(x, y - 1),
        Position(x, y + 1)
      ).filter(!corruptedMemory.contains(_))
    }
    
    /** @return the shortest path through the corrupted memory */
    def dfs(
      end: Position, 
      memory: Set[Position], 
      path: List[Position] = List.empty, 
      shortestPath: Option[List[Position]] = None
    ): Option[List[Position]] = {
      if (this == end) shortestPath.min(path)
      else if (path.size >= visited(this)) None
      else {
        visited.update(this, path.size)
        next(memory).foldLeft((shortestPath)) { case (sp, n) => {
          val nextsp = n.dfs(end, memory, path :+ this, sp)
          if (nextsp.isEmpty) sp else nextsp
        }}
      }    
    }

    extension (shortestPath: Option[List[Position]]) {
      def min(path: List[Position]): Option[List[Position]] = shortestPath match {
        case Some(sp) if (sp.size > path.size) => Some(path)
        case Some(sp) => Some(sp)   
        case None => Some(path)
      }
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

    val source = Source.fromResource(filename)
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
    Day18.visited.clear()
    val shortestPath = start.dfs(end, memory)
    shortestPath.get.size
  }

  /** @return the solution for part2 */
  def part2(corruptedMemory: Set[Position], dimensions: (Int, Int)): Int = {
    require(corruptedMemory.nonEmpty, "corruptedMemory.nonEmpty")
    logger.debug(s"corruptedMemory: ${corruptedMemory}")

    corruptedMemory.size
  }
}
