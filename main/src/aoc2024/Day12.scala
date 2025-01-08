package aoc2024

import com.typesafe.scalalogging.Logger

/** Day12 - Garden Groups
  *
  * @see
  *   https://adventofcode.com/2024/day/12
  *
  * This is going to be interesting. First we have to build/collect the regions.
  * We can do this with flood-fill depth-first traversal.
  *
  * We then need to determine the number of neighbors every cell has. With the
  * number of neighbors we can can calculate the perimeter like this ...
  *
  * For all cells in the region sum up the perimeters of each cell, where the
  * perimeter of each cell is the number of neighbors
  *
  * The area of the region is simply the number of elements in the region.
  *
  * BUT ... we can have multiple regions with the same plant.
  *
  * Means ... while we read the input we should probably collect ...
  *
  * Plot(plant: Char, Position(x: Int, y: Int), neighbors: Int)
  *
  * We can then build the regions by traversing the plots and calculate the area
  * and perimeter for each region. A Region would be ...
  *
  * Region(plant: Char, plots: Seq[Position], area: Int, perimeter: Int)
  *
  * The region would be identified by the top-left plot (which is plots.min).
  *
  * The garden is a Set of Regions.
  *
  * part1:
  *
  *   - read the plots (including the number of neighbors)
  *   - build the regions (with a recursive dfs flood-fill)
  *   - calculate price of the fence from the areas and perimeters of the
  *     regions
  *
  * part2:
  *
  * Notice that the number of corners is the number of sides.
  *
  * Means the only thing that we need to figure out is the number of corners per
  * Plot (or per Region).
  *
  * That is unfortunately not as easy as it sounds. Luckily (tongue-in-cheek)
  * soembody(!!!) has done the hard work already, means we can just use the
  * scala-corner library.
  *
  *   - read the plots (including the number of neighbors)
  *   - build the regions (with a recursive dfs flood-fill)
  *   - calculate the number of corners/sides for each plot
  *   - calculate price of the fence from the areas and the sides of the regions
  */

object Day12:
    val logger: Logger = Logger(this.getClass.getName)

    case class Position(x: Int, y: Int)

    object Position:
        implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))

    /** A Plot with a plant on it. */
    case class Plot(plant: Char, position: Position, neighbors: Set[Position])

    object Plot:
        implicit val ordering: Ordering[Plot] = Ordering.by(_.position)

    /** A Region of Plots. */
    case class Region(
        location: Position,
        plant: Char,
        plots: Set[Position],
        area: Int,
        perimeter: Int,
        sides: Int,
    ):
        require(plant.isLetter, "plant.isLetter")
        require(plots.nonEmpty, "plots.nonEmpty")
        require(area > 0, "area > 0")
        require(perimeter > 0, "perimeter > 0")
    end Region

    type Dimensions = (Int, Int)

    /** A Garden. With all Plots. */
    class Garden(plots: Set[Plot], dimensions: Dimensions):
        val plotsByPlant: Map[Char, Set[Plot]] = plots.groupBy(_.plant)

        /** The regions of the garden. */
        val regions: Set[Region] = plotsByPlant.values
            .foldLeft(Set.empty[Region]): (regions, plots) =>
                regions ++ collectRegions(plots, Set.empty[Region])

        /** Find all regions in the garden. */
        def collectRegion(plots: Set[Plot]): (Region, Set[Plot]) =
            val validPositions = plots.map(_.position)

            def neighbors(p: Position): Set[Position] =
                // format: off
                Set(
                    Position(p.x - 1, p.y),
                    Position(p.x + 1, p.y),
                    Position(p.x, p.y - 1),
                    Position(p.x, p.y + 1),
                )
                // format: on
                    .filter(validPositions.contains)

            /** Do a depth-first traversal flood-fill of the garden from the
              * given start position (to find the region for that start
              * position).
              *
              * @return
              *   the Set of all Positions in the region
              */
            def dfs(position: Position, visited: Set[Position]): Set[Position] =
                if visited.contains(position) then visited
                else
                    val newVisited = visited + position
                    neighbors(position).foldLeft(newVisited): (acc, neighbor) =>
                        dfs(neighbor, acc)
            end dfs

            val location = validPositions.toList.sorted.head
            val regionPositions = dfs(location, Set.empty)
            val regionPlots = plots
                .filter(p => regionPositions.contains(p.position))
            val remainingPositions = validPositions.diff(regionPositions)
            val remainingPlots = plots
                .filter(p => remainingPositions.contains(p.position))
            val area = regionPlots.size
            val perimeter = regionPlots.toList.map(_.neighbors.size).sum
            val sides = countSides(regionPositions, regionPlots.head.plant)

            // format: off
            (Region(location, plots.head.plant, regionPositions, area, perimeter, sides), remainingPlots)
            // format: on
        end collectRegion

        /** @return
          *   the number of sides for the given region (which is given by its
          *   Set of Positions).
          *
          * @note
          *   We can use the scala-corner library to count the number of
          *   corners, because the number of corners is the number of sides.
          */
        def countSides(positions: Set[Position], plant: Char): Int =
            val ps = positions.toList.map: p =>
                val Position(x, y) = p
                ((x, y), plant)

            val corners =
                new corner.CornerCounter(ps.toMap.withDefault(_ => '.')).corners
            corners.map(_._2).sum
        end countSides

        /** @return
          *   (find and return) all regions in the garden, using a flood-fill
          *   algorithm.
          */
        def collectRegions(
            plots: Set[Plot],
            regions: Set[Region],
        ): Set[Region] =
            if plots.isEmpty then regions
            else
                val (region, remainingPlots) = collectRegion(plots)
                collectRegions(remainingPlots, regions + region)

        def price: Int =
            val prices = regions.toList.map: region =>
                region.area * region.perimeter
            prices.sum

        def price0: Int =
            val prices = regions.toList.map: region =>
                region.area * region.sides
            prices.sum
    end Garden

    /** @return the file for the given filename as parsed elements */
    def readFile(filename: String): Garden =
        import scala.io.Source

        def neighbors(p: Position, garden: Array[Array[Char]]): Set[Position] =
            val (maxX, maxY) = (garden.length, garden(0).length)
            val thisPlant = garden(p.x)(p.y)
            val positionsToCheck =
                // format: off
                Set(
                    Position(p.x - 1, p.y),
                    Position(p.x + 1, p.y),
                    Position(p.x, p.y - 1),
                    Position(p.x, p.y + 1),
                )
                // format: on
            val inGardenNeigbors = positionsToCheck
                .filter: p =>
                    p.x >= 0 && p.x < maxX && p.y >= 0 && p.y < maxY
                .filter: p =>
                    val thatPlant = garden(p.x)(p.y)
                    thisPlant != thatPlant

            val outOfGardenNeighbors = positionsToCheck
                .filter: p =>
                    p.x < 0 || p.x >= maxX || p.y < 0 || p.y >= maxY

            inGardenNeigbors ++ outOfGardenNeighbors
        end neighbors

        require(filename.nonEmpty, "filename.nonEmpty")
        logger.debug(s"filename: ${filename}")

        val source = Source.fromResource(filename)
        try
            val garden = source.getLines().toArray.map(_.toCharArray)
            val dimensions = (garden.length, garden(0).length)
            val plots = (0 until dimensions._1).flatMap: x =>
                (0 until dimensions._2).map: y =>
                    val plant = garden(x)(y)
                    val p = Position(x, y)
                    val ns = neighbors(p, garden)
                    Plot(plant, p, ns)

            Garden(plots.toSet, dimensions)
        finally source.close()
        end try
    end readFile

    /** @return the price to fence the garden */
    def part1(garden: Garden): Int =
        require(garden.regions.nonEmpty, "garden.regions.nonEmpty")
        logger.debug(s"garden: ${garden}")

        garden.price
    end part1

    /** @return the solution for part2 */
    def part2(garden: Garden): Int =
        require(garden.regions.nonEmpty, "garden.regions.nonEmpty")
        logger.debug(s"garden: ${garden}")

        garden.price0
    end part2
end Day12
