package aoc2024

import com.typesafe.scalalogging.Logger

/** Day25 - Code Chronicle
  *
  * That looks/sounds simple enough.
  *
  * We just have to find the key/lock combinations that do not overlop (at least for part1), which
  * is easier than finding the key/lock combinations actually fit.
  *
  * Note: Day25 has no part2. Means we can go for the most simple solution possible for part1.
  *
  * There are three data-structures to consider (to model the keys and locks)...
  *
  *   - a 2-dim array
  *   - a list of pin heights
  *   - a set of positions
  *
  * For now I will go with the set of positions, because I can determine if they overlap by
  * intersecting a key-set with a lock-set.
  */

object Day25:
  val logger: Logger = Logger(this.getClass.getName)

  type Position = (Int, Int)

  /** @return the file for the given filename as parsed elements */
  // def readFile(filename: String): (Set[Position], Set[Position]) =
  def readFile(filename: String): (Set[Set[Position]], Set[Set[Position]]) =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    def isKey(block: Seq[String]): Boolean = block(0).forall(_ == '.')

    val source = Source.fromResource(filename)
    try
      val blocks = source.getLines().grouped(8).toSeq
      blocks.foldLeft((Set.empty[Set[Position]], Set.empty[Set[Position]])):
        case ((keys, locks), block) =>
          val positions = block.zipWithIndex.foldLeft(Set.empty[Position]):
            case (positions, (line, row)) => line.zipWithIndex.foldLeft(positions):
                case (positions, (c, col)) => c match
                    case '#' => positions ++ Set((row, col))
                    case '.' => positions

          if isKey(block) then (keys + positions, locks) else (keys, locks + positions)
    finally source.close()
    end try
  end readFile

  /** @return the number non-overlapping key/lock combinations */
  def part1(input: (Set[Set[Position]], Set[Set[Position]])): Int =
    require(input._1.nonEmpty, "input._1.nonEmpty")
    require(input._2.nonEmpty, "input._2.nonEmpty")
    logger.debug(s"input: ${input}")

    val (keys, locks) = input
    val (_, count) = keys.foldLeft(locks, 0):
      case ((ls, count), key) => (ls, count + ls.count(_.intersect(key).isEmpty))

    count
  end part1

  /** @return the solution for part2 */
  def part2(input: (Set[Set[Position]], Set[Set[Position]])): Int =
    require(input._1.nonEmpty, "input._1.nonEmpty")
    require(input._2.nonEmpty, "input._2.nonEmpty")
    logger.debug(s"input: ${input}")

    val (keys, locks) = input
    1
  end part2

end Day25
