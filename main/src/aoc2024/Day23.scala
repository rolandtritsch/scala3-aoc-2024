package aoc2024

import com.typesafe.scalalogging.Logger

/** Day23 - LAN Party */

object Day23:
  val logger: Logger = Logger(this.getClass.getName)

  type Computer = String

  case class Connection(thiz: Computer, thaz: Computer)

  /** @return the Set of connections */
  def readFile(filename: String): Seq[Connection] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        val Array(thiz, thaz) = line.split("-")
        val connection = Connection(thiz, thaz)
        logger.debug(s"connection: ${connection}")
        connection
      }
    finally source.close()
    end try
  end readFile

  /** @return
    *   the number of all sets of three inter-connected computers where at least one computer starts
    *   with 't'
    */
  def part1(connections: Seq[Connection]): Int =
    require(connections.nonEmpty, "connections.nonEmpty")
    logger.debug(s"connections: ${connections}")

    // Build adjacency map
    val graph = connections.foldLeft(Map.empty[String, Set[String]]):
      case (acc, Connection(a, b)) => acc.updatedWith(a)(opt => Some(opt.getOrElse(Set.empty) + b))
          .updatedWith(b)(opt => Some(opt.getOrElse(Set.empty) + a))

    // Find all sets of three inter-connected computers
    val allNodes = graph.keySet
    val triplets =
      for
        a <- allNodes.toSeq
        b <- graph(a)
        c <- graph(a).intersect(graph(b)) if a < b && b < c
      yield Set(a, b, c)

    // Count triplets containing at least one computer starting with 't'
    triplets.count(_.exists(_.startsWith("t")))
  end part1

  /** @return the solution for part2 */
  def part2(connections: Seq[Connection]): Int =
    require(connections.nonEmpty, "connections.nonEmpty")
    logger.debug(s"connections: ${connections}")

    0
  end part2

end Day23
