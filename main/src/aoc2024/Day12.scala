package aoc2024

/** Day12 - Garden Groups
  *
  * @see https://adventofcode.com/2024/day/12
  *
  * This is going to be interesting. First we have to build/collect
  * the regions. We can do this with flood-fill depth-first traversal.
  *
  * We then need to determine the number of neighbors every cell has.
  * With the number of neighbors we can can calculate the perimeter
  * like this ...
  *
  * For all cells in the region sum up the perimeters of each cell,
  * where the perimeter of each cell is the number of neighbors
  *
  * The area of the region is simply the number of elements in the region.
  *
  * BUT ... we can have multiple regions with the same plant.
  *
  * Means ... while we read the input we should probably collect ...
  *
  * Plot(plant: Char, Position(x: Int, y: Int), neighbors: Int) 
  *
  * We can then build the regions by traversing the plots and calculate
  * the area and perimeter for each region. A Region would be ...
  *
  * Region(plant: Char, plots: Seq[Position], area: Int, perimeter: Int)
  * 
  * The region would be identified by the top-left plot (which is plots.min).
  *
  * The garden is a Set of Regions.
  *
  * part1:
  *
  * - read the plots (including the number of neighbors)
  * - build the regions (with a recursive dfs flood-fill)
  * - calculate price of the fence from the areas and perimeters of the regions
  *
  * part2:
  *
  * Notice that the number of corners is the number of sides.
  *
  * Means the only thing that we need to figure out is the number
  * of corners per Plot (or per Region).
  *
  * Let's take a step back. Let's take a look at a 3 by 3 grid and let's see,
  * if we can figure out the number of corners by pattern matching against 
  * a set of patterns.
  * 
  * Note: These patterns need to be rotated 4 times.
  * 
  * Note: For the cells that are on the edge we will create extra cells/padding
  * so that the edge cells are not an "edge" case any more and can be handled
  * like the other cells.
  * 
  * - X the cell must be different
  * - O the cell must be the same
  * - ? the cell can be either
  * 
  * ?X?
  * XOX
  * ?X?
  * 
  * - 4 corners
  * 
  * ?X?
  * OOX
  * ?X?
  * 
  * - 2 corners
  * 
  * ?O?
  * OOX
  * ?X?
  * 
  * - 1 corners
  * 
  * ?X?
  * OOO
  * ?X?
  * 
  * - 0 corners
  * 
  * XOX
  * OOO
  * ?X?
  * 
  * - 2 corners
  * 
  * XOX
  * OOO
  * XOX
  * 
  * - 4 corners
  * 
  * for the cell in the middle from 
  * the number of neighbors and/or the relative positions of the neighbors. 
  * 
  * Note: I think for this to work we also need to take a look at the diagonal 
  * neighbors (and maybe even at the value of the neighbors itself).
  *
  * OXO
  * XOX
  * OXO
  * 
  * - if the number of non-diagonal neighbors is 0, then the number of corners is 4
  * 
  * OOO
  * XOX
  * OXO
  * 
  * - if the number of non-diagonal neighbors is 1 
  *   and the number of diagonal neighbors >= 1, 
  *   then the number of corners is 3
  * 
  * XXX
  * OOO
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are opposite to each other, 
  *   and the number of diagonal neighbors = 0, 
  *   then the number of corners is 0
  * 
  * XOX
  * OOX
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are not opposite to each other, 
  *   and the number of diagonal neighbors = 0, 
  *   then the number of corners is 2
  * 
  * XXO
  * OOO
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are opposite to each other, 
  *   and the number of diagonal neighbors = 1,
  *   then the number of corners is 0
  * 
  * XOO
  * OOX
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are not opposite to each other, 
  *   and the number of diagonal neighbors = 1,
  *   and the neighbor is upper-right or lower-left,  
  *   then the number of corners is 2
  * 
  * OOX
  * OOX
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are not opposite to each other, 
  *   and the number of diagonal neighbors = 1,
  *   and the neighbor is upper-left,  
  *   then the number of corners is 1
  * 
  * XOX
  * OOX
  * XXO
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are not opposite to each other, 
  *   and the number of diagonal neighbors = 1,
  *   and the neighbor is lower-right,  
  *   then the number of corners is 2
  * 
  * XXO
  * OOO
  * OXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are opposite to each other, 
  *   and the number of diagonal neighbors = 2, 
  *   and they are opposite to each other, 
  *   then the number of corners is 0
  * 
  * OXO
  * OOO
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are opposite to each other, 
  *   and the number of diagonal neighbors = 2, 
  *   and they are not opposite to each other, 
  *   then the number of corners is 0
  * 
  * XOO
  * OOX
  * OXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are not opposite to each other, 
  *   and the number of diagonal neighbors = 2, 
  *   and they are opposite to each other, 
  *   then the number of corners is 2
  * 
  * OOO
  * OOX
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 2 
  *   and they are not opposite to each other, 
  *   and the number of diagonal neighbors = 2, 
  *   and they are not opposite to each other, 
  *   then the number of corners is 1
  * 
  * XOX
  * OOO
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 3 
  *   and the number of diagonal neighbors = 0, 
  *   then the number of corners is 2
  * 
  * OOX
  * OOO
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 3 
  *   and the number of diagonal neighbors = 1, 
  *   then the number of corners is 1
  * 
  * XOO
  * OOO
  * OXX
  * 
  * - if the number of non-diagonal neighbors is 3 
  *   and the number of diagonal neighbors = 2,
  *   and they are opposite to each other, 
  *   then the number of corners is 2
  * 
  * OOO
  * OOO
  * XXX
  * 
  * - if the number of non-diagonal neighbors is 3 
  *   and the number of diagonal neighbors = 2,
  *   and they are not opposite to each other, 
  *   then the number of corners is 0
  * 
  * OOO
  * OOO
  * OXX
  * 
  * - if the number of non-diagonal neighbors is 3 
  *   and the number of diagonal neighbors = 3,
  *   then the number of corners is 0
  * 
  * OOO
  * OOO
  * OXO
  * 
  * - if the number of non-diagonal neighbors is 3 
  *   and the number of diagonal neighbors = 4,
  *   then the number of corners is 0
  * 
  * XOX
  * OOO
  * XOX
  * 
  * - if the number of non-diagonal neighbors is 4 
  *   and the number of diagonal neighbors = 0,
  *   then the number of corners is 4
  * 
  * OOX
  * OOO
  * XOX
  * 
  * - if the number of non-diagonal neighbors is 4 
  *   and the number of diagonal neighbors = 1,
  *   then the number of corners is 3
  * 
  * OOX
  * OOO
  * XOO
  * 
  * - if the number of non-diagonal neighbors is 4 
  *   and the number of diagonal neighbors = 2,
  *   then the number of corners is 2
  * 
  * OOO
  * OOO
  * OOX
  * 
  * - if the number of non-diagonal neighbors is 4 
  *   and the number of diagonal neighbors = 3,
  *   then the number of corners is 1
  * 
  * ... and last, but not least ...
  * 
  * - if the number of non-diagonal neighbors is 4 
  *   and the number of diagonal neighbors = 4,
  *   then the number of corners is 0
  * 
  * - if the number of non-diagonal neighbors is 2, then the number of corners is 1
  * - if the number of non-diagonal neighbors is 3, then the number of corners is 0
  * 
  * - if the number of diagonal neighbors is 0, then the number of corners is 4
  *
  * - 0 neighbors means 0 corners
  *
  * - 1 neighbor means 0 corners
  *
  * - 2 neighbors means 1 corner
  *
  * - 3 neighbors means 2 corners
  * 
  */

object Day12 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  case class Position(x: Int, y: Int)

  object Position {
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
  }

  case class Plot(plant: Char, position: Position, neighbors: Set[Position])

  object Plot {
    implicit val ordering: Ordering[Plot] = Ordering.by(_.position)
  }

  case class Region(location: Position, plant: Char, plots: Set[Position], area: Int,  perimeter: Int, sides: Int) {
    require(plant.isLetter, "plant.isLetter")
    require(plots.nonEmpty, "plots.nonEmpty")
    require(area > 0, "area > 0")
    require(perimeter > 0, "perimeter > 0")
  }

  type Dimensions = (Int, Int)

  class Garden (plots: Set[Plot], dimensions: Dimensions) {
    val plotsByPlant = plots.groupBy(_.plant)
    val regions = plotsByPlant.values.foldLeft(Set.empty[Region]) { (regions, plots) =>
      regions ++ collectRegions(plots, Set.empty[Region])
    }

    def collectRegion(plots: Set[Plot]): (Region, Set[Plot]) = {
      val validPositions = plots.map(_.position)

      def neighbors(p: Position): Set[Position] = {
        Set(
          Position(p.x - 1, p.y),
          Position(p.x + 1, p.y),
          Position(p.x, p.y - 1),
          Position(p.x, p.y + 1),
        ).filter(validPositions.contains)
      }

      def dfs(position: Position, visited: Set[Position]): Set[Position] = {
        if (visited.contains(position)) visited
        else {
          val newVisited = visited + position
          neighbors(position).foldLeft(newVisited) { (acc, neighbor) =>
            dfs(neighbor, acc)
          }
        }
      }

      val location = validPositions.toList.sorted.head
      val regionPositions = dfs(location, Set.empty)
      val regionPlots = plots.filter(p => regionPositions.contains(p.position))
      val remainingPositions = validPositions.diff(regionPositions)
      val remainingPlots = plots.filter(p => remainingPositions.contains(p.position))
      val area = regionPlots.size
      val perimeter = regionPlots.toList.map(_.neighbors.size).sum
      //val sides = countSides(regionPlots, regionPositions)
      val sides = countSides1(regionPlots)

      (Region(location, plots.head.plant, regionPositions, area, perimeter, sides), remainingPlots)
    }

    def countSides(rPlots: Set[Plot], rPositions: Set[Position]) = {
      rPlots.toList.map { p => 
        val count = p.neighbors.count(rPositions.contains)
        if(count < 2) 1 else 0
      }.sum
    }

    def countSides1(rPlots: Set[Plot]) = {
      def corners(neighbors: Set[Position]): Int = {
        neighbors.size match {
          case 4 => 0
          case 0 => 4
          case 3 => 2
          case 2 => 1
          case 1 => 2
        }
      }

      rPlots.toList.map(p => corners(p.neighbors)).sum 
    }

    def collectRegions(plots: Set[Plot], regions: Set[Region]): Set[Region] = {
      if(plots.isEmpty) regions
      else {
        val (region, remainingPlots) = collectRegion(plots)
        collectRegions(remainingPlots, regions + region)
      }
    }

    def price: Int = regions.toList.map { region =>
      region.area * region.perimeter
    }.sum

    def price0: Int = regions.toList.map { region =>
      region.area * region.sides
    }.sum
  }

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Garden = {
    import scala.io.Source

    def neighbors(p: Position, garden: Array[Array[Char]]): Set[Position] = {
      val (maxX, maxY) = (garden.size, garden(0).size)
      val thisPlant = garden(p.x)(p.y)
      val positionsToCheck = Set(
        Position(p.x - 1, p.y),
        Position(p.x + 1, p.y),
        Position(p.x, p.y - 1),
        Position(p.x, p.y + 1),
      )
      val inGardenNeigbors = positionsToCheck.filter { p =>
        p.x >= 0 && p.x < maxX && p.y >= 0 && p.y < maxY 
      }.filter { p =>
        val thatPlant = garden(p.x)(p.y)
        thisPlant != thatPlant
      }
      val outOfGardenNeighbors = positionsToCheck.filter { p =>
        p.x < 0 || p.x >= maxX || p.y < 0 || p.y >= maxY 
      }
      inGardenNeigbors ++ outOfGardenNeighbors
    }

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try {
      val garden = source.getLines().toSeq.map { line =>
        line.toCharArray()
      }.toArray
      val dimensions = (garden.size, garden(0).size)
      val plots = (0 until dimensions._1).flatMap { x => 
        (0 until dimensions._2).map { y =>
          val plant = garden(x)(y)
          val p = Position(x, y)
          val ns = neighbors(p, garden)
          Plot(plant, p, ns)
        }
      }.toSet

      Garden(plots, dimensions)
    } finally {
      source.close()
    }
  }

  /** @return the price to fence the garden */
  def part1(garden: Garden): Int = {
    require(garden.regions.nonEmpty, "garden.regions.nonEmpty")
    logger.debug(s"garden: ${garden}")
  
    garden.price
  }

  /** @return the solution for part2 */
  def part2(garden: Garden): Int = {
    require(garden.regions.nonEmpty, "garden.regions.nonEmpty")
    logger.debug(s"garden: ${garden}")

    garden.price0
  }
}
