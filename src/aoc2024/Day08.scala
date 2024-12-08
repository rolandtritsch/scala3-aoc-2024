package aoc2024

/** Day08 - Resonant Collinearity
  *
  * That's an interesting one.
  *
  * - Let's get the Set of antennas
  * - For every possible pair of antennas with the same frequency
  *   (and different positions) we need to calculate the pair
  *   of antinodes
  * - We then need to count the antinodes (but just the ones
  *   that are within the boundaries of the grid
  */

object Day08 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  enum Positioning {
    case UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT
  }
  import Positioning._

  case class Position(val x: Int, val y: Int)
  case class Antenna(val frequency: Char, val position: Position) {
    def positioning(a: Antenna): Positioning = {
      if(position.x > a.position.x && position.y > a.position.y) LOWER_RIGHT
      else if(position.x > a.position.x && position.y < a.position.y) LOWER_LEFT
      else if(position.x < a.position.x && position.y > a.position.y) UPPER_RIGHT
      else if(position.x < a.position.x && position.y < a.position.y) UPPER_LEFT
      else throw new RuntimeException("Unexpected case")
    }

    private def diffX(a: Antenna) = math.abs(position.x - a.position.x)
    private def diffY(a: Antenna) = math.abs(position.y - a.position.y)

    def antiNode(a: Antenna): Position = positioning(a)  match {
        case LOWER_RIGHT => Position(position.x + diffX(a), position.y + diffY(a))
        case LOWER_LEFT => Position(position.x + diffX(a), position.y - diffY(a))
        case UPPER_RIGHT => Position(position.x - diffX(a), position.y + diffY(a))
        case UPPER_LEFT => Position(position.x - diffX(a), position.y - diffY(a))
      }
  }
  case class Grid(val antennas: Set[Antenna], val dimensions: (Int, Int)) {
    def isOnGrid(position: Position): Boolean = {
      val (maxX, maxY) = dimensions
      (0 until maxX).contains(position.x) && (0 until maxY).contains(position.y) 
    }
  }

  extension (antennas: Set[Antenna]) {
    def pairs: Set[(Antenna, Antenna)] = {
      val groupByFrequency = antennas.groupBy(_.frequency).map(_._2)
      val antennaPairs = groupByFrequency.map { antennasForOneFrequency => {
        antennasForOneFrequency.subsets(2).map { antennaPair => {
          val pair = antennaPair.toSeq
          (pair(0), pair(1))
        }}
      }}
      antennaPairs.flatten.toSet
    }
  }

  extension (pair: (Antenna, Antenna)) {
    def antiNodes: Set[Position] = {
      val (a1, a2) = pair
      Set(a1.antiNode(a2), a2.antiNode(a1))
    }
  }

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Grid = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      val lines = source.getLines.toSeq
      val antennas = lines.zipWithIndex.map { (line, x) => {
        logger.debug(s"line: ${line}")

        line.toSeq.zipWithIndex.flatMap { (char, y) => char match {
          case '.' => None
          case c => Some(Antenna(c, Position(x, y)))
        }}
      }}.flatten.toSet

      val maxX = lines.size
      val maxY = lines(0).size
      Grid(antennas, (maxX, maxY))
    } finally {
      source.close()
    }
  }

  /** @return the number of antinodes on the grid */
  def part1(grid: Grid): Int = {
    require(grid.antennas.nonEmpty, "grid.antennas.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val antennaPairs = grid.antennas.pairs
    val antiNodes = antennaPairs.map(_.antiNodes).flatten
    antiNodes.count { node => {
      grid.isOnGrid(node)
    }}
  }

  /** @return the solution for part2 */
  def part2(grid: Grid): Int = {
    require(grid.antennas.nonEmpty, "grid.antennas.nonEmpty")
    logger.debug(s"grid: ${grid}")

    grid.dimensions._1
  }
}
