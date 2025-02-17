package util

import com.typesafe.scalalogging.Logger

/** A Grid. The datastructure we use to read a grid from a file.
  *
  * The file has ...
  *
  *   - a number of rows and a number of columns
  *   - every position is either a free space (.) or an obstacle (#) or the start (S) or the end (E)
  *   - the start and end positions are optional
  *   - the dimensions of the grid are detemined by the number of rows and columns
  */
class Grid(
    val free: Set[Position],
    val blocked: Set[Position],
    val start: Option[Position],
    val end: Option[Position],
    val dimensions: (Int, Int),
):
  import util.DPosition.*

  val logger: Logger = Logger(this.getClass.getName)

  def clone(
      free: Set[Position] = this.free,
      blocked: Set[Position] = this.blocked,
      start: Option[Position] = this.start,
      end: Option[Position] = this.end,
      dimensions: (Int, Int) = this.dimensions,
  ): Grid = Grid(free, blocked, start, end, dimensions)

  override def toString: String =
    s"Grid(free: ${free}, blocked: ${blocked}, start: ${start}, end: ${end}, dimensions: ${dimensions})"

  def toStringPretty(current: Option[Position] = None, path: List[Position] = List.empty): String =
    val (dimX, dimY) = dimensions
    val cols = (0 until dimX).map: x =>
      val rows = (0 until dimY).map: y =>
        val pos = Position(x, y)
        if current.getOrElse(Position(-1, -1)) == pos then 'X'
        else if start.getOrElse(Position(-1, -1)) == pos then 'S'
        else if end.getOrElse(Position(-1, -1)) == pos then 'E'
        else if path.contains(pos) then 'O'
        else if blocked.contains(pos) then '#'
        else if free.contains(pos) then '.'
        else new RuntimeException("Unexpected case")
        end if
      rows.mkString
    val grid = cols.mkString("\n")
    s"${this}\n${grid}\n"
  end toStringPretty

  def neighbors: Map[Position, Set[DPosition]] = free.map(p => (p, adjacent(p))).toMap

  def adjacent(p: Position, visited: Set[Position] = Set.empty): Set[DPosition] = p.toDPosition
    .adjacent.filter(p => free.contains(p.toPosition) && !visited.contains(p.toPosition))
  end adjacent

end Grid

object Grid:
  val logger: Logger = Logger(this.getClass.getName)

  trait GridFactory[G]:

    def create(
        free: Set[Position],
        blocked: Set[Position],
        start: Option[Position],
        end: Option[Position],
        dimensions: (Int, Int),
    ): G

  end GridFactory

  def fromResource[G](filename: String)(using factory: GridFactory[G]): G =
    val source = scala.io.Source.fromResource(filename)
    try
      val init = (
        Set.empty[Position],
        Set.empty[Position],
        Option.empty[Position],
        Option.empty[Position],
        (0, 0),
      )
      val (free, blocked, start, end, max) = source.getLines().toSeq.zipWithIndex.foldLeft(init):
        case (grid, (line, x)) =>
          logger.debug(s"grid: ${grid}, line: ${line}")
          line.zipWithIndex.foldLeft(grid):
            case (grid, (c, y)) =>
              val (free, blocked, start, end, _) = grid
              c match
                case '.' => (free + Position(x, y), blocked, start, end, (x, y))
                case '#' => (free, blocked + Position(x, y), start, end, (x, y))
                case 'S' => (free + Position(x, y), blocked, Some(Position(x, y)), end, (x, y))
                case 'E' => (free + Position(x, y), blocked, start, Some(Position(x, y)), (x, y))
                case _   => throw new RuntimeException("Unexpected case")
              end match

      val (maxX, maxY) = max
      factory.create(free, blocked, start, end, (maxX + 1, maxY + 1))
    finally source.close()
    end try
  end fromResource

  object Factory:

    given GridFactory[Grid] with

      def create(
          free: Set[Position],
          blocked: Set[Position],
          start: Option[Position],
          end: Option[Position],
          dimensions: (Int, Int),
      ): Grid = new Grid(free, blocked, start, end, dimensions)

    end given

  end Factory

end Grid
