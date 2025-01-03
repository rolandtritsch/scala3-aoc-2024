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
  * That is unfortunately not as easy as it sounds. Luckily (tongue-in-cheek)
  * soembody(!!!) has done the hard work already, means we can just use
  * the scala-corner library.
  *
  * - read the plots (including the number of neighbors)
  * - build the regions (with a recursive dfs flood-fill)
  * - calculate the number of corners/sides for each plot
  * - calculate price of the fence from the areas and the sides of the regions
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
      val sides = countSides(regionPositions, regionPlots.head.plant)

      (Region(location, plots.head.plant, regionPositions, area, perimeter, sides), remainingPlots)
    }

    def countSides(positions: Set[Position], plant: Char) = {
      val ps = positions.toList.map { case Position(x, y) => {
        ((x, y), plant) 
      }}.toMap.withDefault(_ => '.')
      new corner.CornerCounter(ps).corners.map(_._2).sum
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
