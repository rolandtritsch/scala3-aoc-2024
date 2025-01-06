package util

/** A Grid. The datastructure we use to read a grid from a file.
  *
  * The file has ...
  *
  *   - a number of rows and a number of columns
  *   - every position is either a free space (.) or an obstacle (#) or the
  *     start (S) or the end (E)
  *   - the start and end positions are optional
  *   - the dimensions of the grid are detemined by the number of rows and
  *     columns
  */
class Grid(
  val free: Set[Position],
  val blocked: Set[Position],
  val start: Option[Position],
  val end: Option[Position],
  val dimensions: (Int, Int),
):
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  def clone(
    free: Set[Position] = this.free,
    blocked: Set[Position] = this.blocked,
    start: Option[Position] = this.start,
    end: Option[Position] = this.end,
    dimensions: (Int, Int) = this.dimensions,
  ): Grid = Grid(free, blocked, start, end, dimensions)

  override def toString: String =
    s"Grid(free: ${free}, blocked: ${blocked}, start: ${start}, end: ${end}, dimensions: ${dimensions})"

  def toStringPretty(
    current: Option[Position] = None,
    path: List[Position] = List.empty,
  ): String =
    val (dimX, dimY) = dimensions
    val grid         = (0 until dimX).map { x =>
      (0 until dimY).map { y =>
        val pos = Position(x, y)
        if current.getOrElse(Position(-1, -1)) == pos then 'X'
        else if start.getOrElse(Position(-1, -1)) == pos then 'S'
        else if end.getOrElse(Position(-1, -1)) == pos then 'E'
        else if path.contains(pos) then 'O'
        else if blocked.contains(pos) then '#'
        else if free.contains(pos) then '.'
        else new RuntimeException("Unexpected case")
      }.mkString
    }.mkString.mkString("\n")
    s"${this}\n${grid}\n"

  def neighbors: Map[Position, Set[Position]] = free.map(p => (p, adjacent(p)))
    .toMap

  def adjacent(p: Position, visited: Set[Position] = Set.empty): Set[Position] =
    Set(
      Position(p.x - 1, p.y),
      Position(p.x + 1, p.y),
      Position(p.x, p.y - 1),
      Position(p.x, p.y + 1),
    ).filter(p => free.contains(p) && !visited.contains(p))

object Grid:

  trait GridFactory[G]:

    def create(
      free: Set[Position],
      blocked: Set[Position],
      start: Option[Position],
      end: Option[Position],
      dimensions: (Int, Int),
    ): G

  def fromResource[G](filename: String)(using factory: GridFactory[G]): G =
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    val source = scala.io.Source.fromResource(filename)
    try
      val init                             = (
        Set.empty[Position],
        Set.empty[Position],
        Option.empty[Position],
        Option.empty[Position],
        (0, 0),
      )
      val (free, blocked, start, end, max) = source.getLines().toSeq
        .zipWithIndex.foldLeft(init) { case (grid, (line, x)) =>
          logger.debug(s"grid: ${grid}, line: ${line}")

          line.zipWithIndex.foldLeft(grid) { case (grid, (c, y)) =>
            val (free, blocked, start, end, _) = grid
            c match
              case '.' => (free + Position(x, y), blocked, start, end, (x, y))
              case '#' => (free, blocked + Position(x, y), start, end, (x, y))
              case 'S' => (
                  free + Position(x, y),
                  blocked,
                  Some(Position(x, y)),
                  end,
                  (x, y),
                )
              case 'E' => (
                  free + Position(x, y),
                  blocked,
                  start,
                  Some(Position(x, y)),
                  (x, y),
                )
              case _   => throw new RuntimeException("Unexpected case")
          }
        }

      val (maxX, maxY) = max
      factory.create(free, blocked, start, end, (maxX + 1, maxY + 1))
    finally source.close()

  object Factory:

    given GridFactory[Grid] with

      def create(
        free: Set[Position],
        blocked: Set[Position],
        start: Option[Position],
        end: Option[Position],
        dimensions: (Int, Int),
      ): Grid = new Grid(free, blocked, start, end, dimensions)
