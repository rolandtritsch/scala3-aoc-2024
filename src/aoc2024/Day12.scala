package aoc2024

/** Day12 - Garden Groups
  *
  * @see https://adventofcode.com/2024/day/12
  *
  * This is going to be interesting. First we have to build/collect
  * the regions. We can do this with flood-fill depth-first traversal.
  *
  * We then need to determine the number of neighbours every cell has.
  * With the number of neighbours we can can calculate the perimeter
  * like this ...
  *
  * For all cells in the region sum up the perimeters of each cell,
  * where the perimeter of each cell is the number of neighbours
  *
  * The area of the region is simply the number of elements in the region.
  *
  * BUT ... we can have multiple regions with the same plant.
  *
  * Means ... while we read the input we should probably collect ...
  *
  * Plot(plant: Char, Position(x: Int, y: Int), neighbours: Int) 
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
  * - read the plots (including the number of neighbours)
  * - build the regions
  * - calculate price of the fence from the areas and perimeters of the regions 
  */

object Day12 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  case class Position(x: Int, y: Int) {
    require(x >= 0, "x >= 0")
    require(y >= 0, "y >= 0")

    def neighbours(dimension: Dimensions): List[Position] = {
      val (maxX, maxY) = dimension
      List(
        (x - 1, y),
        (x + 1, y),
        (x, y - 1),
        (x, y + 1),
      ).filter { (x, y) =>
        x >= 0 && x < maxX && y >= 0 && y < maxY
      }.map { (x, y) =>
        Position(x, y)
      }.sorted
    }
  }

  object Position {
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
  }

  case class Plot(plant: Char, position: Position, neighbours: Int) {
    require(plant.isLetter, "plant.isLetter")
    require(neighbours >= 0, "neighbours >= 0")
  }

  object Plot {
    implicit val ordering: Ordering[Plot] = Ordering.by(_.position)
  }

  case class Region(location: Position, plant: Char, plots: List[Position], area: Int,  perimeter: Int) {
    require(plant.isLetter, "plant.isLetter")
    require(plots.nonEmpty, "plots.nonEmpty")
    require(area > 0, "area > 0")
    require(perimeter > 0, "perimeter > 0")
  }

  type Dimensions = (Int, Int)

  class Garden (plots: List[Plot], dimensions: Dimensions) {
    val plotsByPlant = plots.groupBy(_.plant)

    // def price: Int = regions.map { region =>
    //   region.area * region.perimeter
    // }.sum

    // override def toString: String = {
    //   val regionsString = regions.map { region =>
    //     s"Region(${region.location}, ${region.plant}, ${region.area}, ${region.perimeter})"
    //   }.mkString(", ")
    //   s"Garden(${dimensions})(${regionsString})"
    // }
  }

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Garden = {
    import scala.io.Source

    def neighbours(thisPosition: Position, garden: Array[Array[Char]]): Int = {
      val dimensions = (garden.size, garden(0).size)
      val thisPlant = garden(thisPosition.x)(thisPosition.y)
      thisPosition.neighbours(dimensions).count { thatPosition =>
        val Position(x, y) = thatPosition
        val thatPlant = garden(x)(y)
        thisPlant != thatPlant
      }
    }

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      val garden = source.getLines().toSeq.map { line =>
        line.toCharArray()
      }.toArray
      val dimensions = (garden.size, garden(0).size)
      val plots = (0 until dimensions._1).flatMap { x => 
        (0 until dimensions._2).map { y =>
          val plant = garden(x)(y)
          val p = Position(x, y)
          val n = neighbours(p, garden)
          Plot(plant, p, n)
        }
      }.toList

      Garden(plots, dimensions)
    } finally {
      source.close()
    }
  }

  /** @return the price to fence the garden */
  def part1(garden: Garden): Int = {
    //require(garden.regions.nonEmpty, "garden.regions.nonEmpty")
    logger.info(s"garden: ${garden}")
  
    garden.plotsByPlant.size
  }

  /** @return the solution for part2 */
  def part2(garden: Garden): Int = {
    //require(garden.regions.nonEmpty, "garden.regions.nonEmpty")
    logger.debug(s"garden: ${garden}")

    garden.plotsByPlant.size
  }
}
