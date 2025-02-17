package aoc2024

import com.typesafe.scalalogging.Logger

/** Day08 - Resonant Collinearity
  *
  * That's an interesting one.
  *
  * part1:
  *
  *   - Let's get the Set of antennas
  *   - For every possible pair of antennas with the same frequency (and different positions) we
  *     need to calculate the pair of antinodes
  *   - We then need to count the antinodes (but just the ones that are within the boundaries of the
  *     grid)
  *
  * part2:
  *
  *   - ooohhh ... we need to find more antiNodes
  *   - let's do this recursively (using antiNode; until we leave the grid)
  *   - we then just need to merge the antiNodePositions with the antennaPositions
  *   - note: Luckily we decided to use Sets (that way the merge will eliminate dublicates)
  *   - and count them
  *
  * Done.
  */

object Day08:
  val logger: Logger = Logger(this.getClass.getName)

  /** Relative positioning of one antenna to the second one in the pair */
  enum Positioning:
    case UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT

  import Positioning.*

  /** A position (for antennas and antiNodes) */
  case class Position(x: Int, y: Int)

  /** An antenna */
  case class Antenna(frequency: Char, position: Position):

    /** @return the relative positioning of one Antenna to the second one */
    private def positioning(a: Antenna): Positioning =
      if position.x > a.position.x && position.y > a.position.y then LOWER_RIGHT
      else if position.x > a.position.x && position.y < a.position.y then LOWER_LEFT
      else if position.x < a.position.x && position.y > a.position.y then UPPER_RIGHT
      else if position.x < a.position.x && position.y < a.position.y then UPPER_LEFT
      else throw new RuntimeException("Unexpected case")
    end positioning

    private def diffX(a: Antenna) = math.abs(position.x - a.position.x)
    private def diffY(a: Antenna) = math.abs(position.y - a.position.y)

    /** @return the position of antiNode for the given antenna */
    def antiNode(a: Antenna): Position = positioning(a) match
      case LOWER_RIGHT => Position(position.x + diffX(a), position.y + diffY(a))
      case LOWER_LEFT  => Position(position.x + diffX(a), position.y - diffY(a))
      case UPPER_RIGHT => Position(position.x - diffX(a), position.y + diffY(a))
      case UPPER_LEFT  => Position(position.x - diffX(a), position.y - diffY(a))
    end antiNode

  end Antenna

  /** The grid. */
  class Grid(val antennas: Set[Antenna], val dimensions: (Int, Int)):

    /** return true, if the given position is on the grid */
    def isOnGrid(position: Position): Boolean =
      val (maxX, maxY) = dimensions
      Grid.isOnGrid(maxX, maxY, position)

  end Grid

  object Grid:

    def isOnGrid(maxX: Int, maxY: Int, position: Position): Boolean = (0 until maxX)
      .contains(position.x) && (0 until maxY).contains(position.y)

  end Grid

  extension (antennas: Set[Antenna])

    // format: off
    /** @return the set of antenna pairs for all the given antennas. */ // scalafix:ok
    // format: on
    def pairs: Set[(Antenna, Antenna)] =
      val groupByFrequency = antennas.groupBy(_.frequency).map(_._2)
      val antennaPairs = groupByFrequency.flatMap: antennasForOneFrequency =>
        antennasForOneFrequency.subsets(2).map: antennaPair =>
          val pair = antennaPair.toSeq
          (pair(0), pair(1))
      antennaPairs.toSet
    end pairs

  end extension

  extension (pair: (Antenna, Antenna))

    // format: off
    /** @return the antiNodes for the antenna pair. */ // scalafix:ok
    // format: on
    def antiNodes: Set[Position] =
      val (a1, a2) = pair
      Set(a1.antiNode(a2), a2.antiNode(a1))

    /** @return the antiNodes for the antenna pair (recursively). */
    def antiNodes0(dimensions: (Int, Int)): Set[Position] =
      val (maxX, maxY) = dimensions

      def collectAntiNodes(a1: Antenna, a2: Antenna, nodes: Set[Position]): Set[Position] =
        logger.debug(s"a1: ${a1}, a2: ${a2}, nodes: ${nodes}")
        val nextAntiNodePosition = a1.antiNode(a2)
        logger.debug(s"nextAntiNodePosition: ${nextAntiNodePosition}")

        if !Grid.isOnGrid(maxX, maxY, nextAntiNodePosition) then nodes
        else
          collectAntiNodes(
            Antenna(a1.frequency, nextAntiNodePosition),
            a1,
            nodes + nextAntiNodePosition,
          )
        end if
      end collectAntiNodes

      val (a1, a2) = pair
      collectAntiNodes(a1, a2, Set()) ++ collectAntiNodes(a2, a1, Set())
    end antiNodes0

  end extension

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Grid =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val lines = source.getLines.toSeq
      val antennas = lines.zipWithIndex.flatMap: (line, x) =>
        logger.debug(s"line: ${line}")

        line.toSeq.zipWithIndex.flatMap: (char, y) =>
          char match
            case '.' => None
            case c   => Some(Antenna(c, Position(x, y)))
      end antennas

      val maxX = lines.size
      val maxY = lines(0).size
      Grid(antennas.toSet, (maxX, maxY))
    finally source.close()
    end try
  end readFile

  /** @return the number of antinodes on the grid. */
  def part1(grid: Grid): Int =
    require(grid.antennas.nonEmpty, "grid.antennas.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val antennaPairs = grid.antennas.pairs
    val antiNodePositions = antennaPairs.flatMap(_.antiNodes)
    antiNodePositions.count(grid.isOnGrid)
  end part1

  /** @return the number of antinodes on the grid (recursively). */
  def part2(grid: Grid): Int =
    require(grid.antennas.nonEmpty, "grid.antennas.nonEmpty")
    logger.debug(s"grid: ${grid}")

    val antennaPairs = grid.antennas.pairs
    val antiNodePositions = antennaPairs.flatMap(_.antiNodes0(grid.dimensions))
    val antennaPositions = grid.antennas.map(_.position)
    (antiNodePositions ++ antennaPositions).size
  end part2

end Day08
