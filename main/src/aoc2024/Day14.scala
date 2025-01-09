package aoc2024

import com.typesafe.scalalogging.Logger

/** Day14 - Restroom Redoubt
  *
  * @see
  *   https://adventofcode.com/2024/day/14
  *
  * OK ... we have Robots and Positions and Velocities. We can go with a 2-dimensional array or with
  * case classes. I think I'll go with case classes.
  *
  * part1:
  *
  *   - Read the input file
  *   - Move the robots around for 100 seconds
  *   - Get the quadrants and count the number of robots in each
  *   - Multiply the number of robots in each quadrant
  *
  * part2:
  *
  * Not sure yet. Not clear how the christmas tree should look like. After I find out how the
  * christmas tree should look like, we can run the simulation one step at a time to find out of if
  * the result matches the shape of the christmas tree.
  *
  * One assumption that can be made is that the tree is in a frame of positions with at least one
  * robot. We can try to find this frame by looking for a 5x5 grid with an L shape in it (all x=0
  * and y=0 are occupied by at least one robot), rotate that grid 4 times and try to find a
  * simulation that has all 4 grids on it (find one and then check (based on the x and y), if the
  * other ones are there too).
  */

object Day14:
  val logger: Logger = Logger(this.getClass.getName)

  /** A position in the 2-dimensional space */
  case class Position(x: Int, y: Int):

    private[Day14] def add(v: Velocity, dimensions: (Int, Int)): Position =
      val (maxX, maxY) = dimensions
      val newX =
        if this.x + v.dx >= maxX then this.x + v.dx - maxX
        else if this.x + v.dx < 0 then this.x + v.dx + maxX
        else this.x + v.dx
      val newY =
        if this.y + v.dy >= maxY then this.y + v.dy - maxY
        else if this.y + v.dy < 0 then this.y + v.dy + maxY
        else this.y + v.dy
      Position(newX, newY)
    end add

  end Position

  object Position:
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))

  import scala.math.Ordering.Implicits.*

  /** A velocity in the 2-dimensional space */
  case class Velocity(dx: Int, dy: Int)

  /** A robot with an id, a position and a velocity */
  case class Robot(id: Int, position: Position, velocity: Velocity)

  extension (robots: Set[Robot])

    def move(dimensions: (Int, Int)): Set[Robot] = robots.map: robot =>
      val newPosition = robot.position.add(robot.velocity, dimensions)
      Robot(robot.id, newPosition, robot.velocity)

  /** A Simulation with a Set of Robots (and the Dimensions of the Simulation)
    */
  class Simulation(robots: Set[Robot], dimensions: (Int, Int)):

    /** @return a new Simulation after running for the given seconds */
    def run(seconds: Int): Simulation =
      val newRobots = (1 to seconds).foldLeft(robots): (rs, _) =>
        rs.move(dimensions)

      Simulation(newRobots, dimensions)
    end run

    /** @return the number of Robots in each quadrant */
    def quadrants(): Map[(Int, Int), Int] =
      def isValid(robot: Robot): Boolean =
        val (midX, midY) = (dimensions._1 / 2, dimensions._2 / 2)
        robot.position.x != midX && robot.position.y != midY

      def byQuadrant(robot: Robot): (Int, Int) =
        val (midX, midY) = (dimensions._1 / 2, dimensions._2 / 2)
        val qX = if robot.position.x < midX then 0 else 1
        val qY = if robot.position.y < midY then 0 else 1
        (qX, qY)
      end byQuadrant

      robots.filter(isValid).groupBy(byQuadrant).map: (k, v) =>
        (k, v.size)
    end quadrants

    override def toString(): String =
      val (maxX, maxY) = dimensions
      val rs = robots.toList.sortBy(_.position).map { r =>
        s"Robot(${r.id}, ${r.position}, ${r.velocity})"
      }.mkString("\n")
      s"Simulation(${robots.size}/${dimensions}):\n${rs}\n"
    end toString

    def toStringPretty(): String =
      val (maxX, maxY) = dimensions
      val rs = (0 until maxX).map { x =>
        (0 until maxY).map(y => robots.count(_.position == Position(x, y))).mkString
      }.mkString("\n")
      s"Simulation(${robots.size}/${dimensions}):\n${rs}\n"
    end toStringPretty

  end Simulation

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Simulation =
    import scala.io.Source
    import scala.util.matching

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    var dimensions = (0, 0)
    val source = Source.fromResource(filename)
    try
      val robots = source.getLines().toSeq.zipWithIndex.map: (line, robot) =>
        logger.debug(s"line: ${line}")

        // p=0,4 v=3,-3
        val parser: matching.Regex = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r
        val parsed = parser.findAllIn(line).matchData.next.subgroups.map(_.toInt)
        assert(parsed.size == 4, s"parsed.size == 4: ${parsed.size}")

        val (py, px, vy, vx) = (parsed(0), parsed(1), parsed(2), parsed(3))

        val (maxX, maxY) = dimensions
        dimensions = (math.max(maxX, px + 1), math.max(maxY, py + 1))

        Robot(robot, Position(px, py), Velocity(vx, vy))

      Simulation(robots.toSet, dimensions)
    finally source.close()
    end try
  end readFile

  /** @return the safety factor */
  def part1(simulation: Simulation): Int =
    // require(simulation.robots.nonEmpty, "simulation.robots.nonEmpty")
    logger.debug(s"simulation: ${simulation}")

    simulation.run(100).quadrants().values.product
  end part1

  /** @return the solution for part2 */
  def part2(simulation: Simulation): Int =
    // require(simulation.robots.nonEmpty, "simulation.robots.nonEmpty")
    logger.debug(s"simulation: ${simulation}")

    simulation.run(100).quadrants().values.product
  end part2

end Day14
