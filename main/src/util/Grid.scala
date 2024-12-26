package util

/** A Grid. Defined by a set of obstacles and an end position.
  */
class Grid(val obstacles: Set[Position], val end: Position) {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  def this() = this(Set.empty, Position(0, 0))

  def clone(
      obstacles: Set[Position] = this.obstacles,
      end: Position = this.end
  ): Grid = {
    Grid(obstacles, end)
  }

  override def toString: String = {
    s"Grid(obstacles=${obstacles}, end=${end})"
  }

  /** @return
    *   a new Grid that is surrounded by obstacles. That way we do not have to
    *   constantly check for the boundaries of the grid.
    */
  def surround(): Grid = {
    val dimX = obstacles.map { case Position(x, _) => x }.max + 1
    val dimY = obstacles.map { case Position(_, y) => y }.max + 1

    surround((dimX, dimY))
  }

  /** @return
    *   new Grid that is surrounded by obstacles (with the given dimensions).
    */
  def surround(dimensions: (Int, Int)): Grid = {
    val (dimX, dimY) = dimensions

    val left = (0 until dimX).map { x => Position(x, -1) }
    val right = (0 until dimX).map { x => Position(x, dimY) }
    val top = (0 until dimY).map { y => Position(-1, y) }
    val bottom = (0 until dimY).map { y => Position(dimX, y) }

    Grid(obstacles ++ left ++ right ++ top ++ bottom, end)
  }
}

extension (source: scala.io.BufferedSource) {
  def grid: Grid = {
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    try {
      source.getLines().toSeq.zipWithIndex.foldLeft(new Grid()) {
        case (grid, (line, x)) => {
          logger.debug(s"line: ${line}")

          line.zipWithIndex.foldLeft(grid) { case (grid, (c, y)) =>
            c match {
              case '#' =>
                grid.clone(obstacles = grid.obstacles + Position(x, y))
              case 'E' => grid.clone(end = Position(x, y))
              case _   => grid
            }
          }
        }
      }
    } finally {
      source.close()
    }
  }
}
