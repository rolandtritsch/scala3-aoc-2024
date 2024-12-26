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

  def toStringPretty(current: Position, path: List[Position]): String = {
    val grid =
        (dimensions.minX to dimensions.maxX).map { x =>
          (dimensions.minY to dimensions.maxY).map { y => {
            val pos = Position(x, y)
            if (current == pos) 'X'
            else if (end == pos) 'E'
            else if (path.contains(pos)) 'O'
            else if (obstacles.contains(pos)) '#'
            else '.'
          }
        }.mkString
      }.mkString("\n")
      s"${this}\n${grid}\n"
  }

  case class Dimensions(minX: Int, minY: Int, maxX: Int, maxY: Int)

  private def dimensions: Dimensions = {
    val minX = obstacles.map(_.x).min
    val minY = obstacles.map(_.y).min
    val maxX = obstacles.map(_.x).max
    val maxY = obstacles.map(_.y).max
    Dimensions(minX, minY, maxX, maxY)
  }

  /** @return
    *   a new Grid that is surrounded by obstacles. That way we do not have to
    *   constantly check for the boundaries of the grid.
    */
  def surround(): Grid = {
    surround(dimensions)
  }

  /** @return
    *   new Grid that is surrounded by obstacles (with the given dimensions).
    */
  def surround(dimensions: Dimensions): Grid = {
    val corners = Set(
      Position(dimensions.minX - 1, dimensions.minY - 1),
      Position(dimensions.maxX + 1, dimensions.minY - 1),
      Position(dimensions.maxX + 1, dimensions.maxY + 1),
      Position(dimensions.minX - 1, dimensions.maxY + 1)
    )

    val left = (dimensions.minX to dimensions.maxX).map { x => Position(x, dimensions.minY - 1) }
    val right = (dimensions.minX to dimensions.maxX).map { x => Position(x, dimensions.maxY + 1) }
    val top = (dimensions.minY to dimensions.maxY).map { y => Position(dimensions.minX - 1, y) }
    val bottom = (dimensions.minY to dimensions.maxY).map { y => Position(dimensions.maxX + 1, y) }

    Grid(obstacles ++ left ++ right ++ top ++ bottom ++ corners, end)
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
