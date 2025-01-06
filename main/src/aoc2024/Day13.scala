package aoc2024

/** Day13 - Claw Contraption
  *
  * Let first tackle reading the input file. It comes in blocks of 4 lines ...
  * --- Button A: X+69, Y+23 Button B: X+27, Y+71 Prize: X=18641, Y=10279
  *
  * ---
  *
  * Let's group the lines into groups of 4 and merge the groups into one String
  * and regex on that String to extract the configuration of the ClawMachine.
  *
  * We need a Position (again). The ClawMachine will be something like this ...
  *
  * case class ClawMachine(diffA: Position, diffB: Position, prize: Position)
  *
  * part1:
  *
  *   - build the first 100 combinations of pressing A and B with a recursive
  *     function
  *   - try them all and find the once that will win you the price (if any)
  *   - for the combinations what win, find the one that is cheapest
  *
  * Note: Think about part2 ...
  *
  *   - what if you need to find the most expensive way to win the price
  *   - what if there is not restriction on the number of combinations
  *   - ...
  *
  * Part2:
  *
  * LOL ... it is one of these again ...
  *
  * Tried running my solution with a depth of Int.MaxValue and ... (you can
  * guess what happened).
  *
  * Now I am thinking that for part2 we need to think about this differently.
  *
  * Maybe with some algebra. It is not hard to see that ...
  *
  * Button A: X+94, Y+34 Button B: X+22, Y+67 Prize: X=8400, Y=5400
  *
  * ... is ...
  *
  *   - A * 94 + B * 22 = 8400
  *   - A * 34 + B * 67 = 5400
  *
  * ... and then :) ...
  *
  *   - using breeze
  *   - making sure to only consider solutions that are positive and are whole
  *     numbers
  */

object Day13:
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Token = Long

  case class Position(x: Long, y: Long):
    def +(diff: Position): Position = Position(x + diff.x, y + diff.y)

  object Position:
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
  import scala.math.Ordering.Implicits.*

  case class Button(diff: Position, cost: Token)

  case class ClawMachine(A: Button, B: Button, target: Position):

    def cheapestWayToWin(depth: Int): Option[Long] =
      import scala.collection.immutable.MultiSet

      val visited = scala.collection.mutable.Set[MultiSet[Token]]()

      def loop(
        position: Position,
        way: MultiSet[Token],
        ways: Set[MultiSet[Token]],
        minCost: Option[Long],
        depth: Int,
      ): (Set[MultiSet[Token]], Option[Long]) =
        logger.debug(s"position: ${position}, way: ${way}, ways: ${ways}, minCost: ${minCost}, depth: ${depth}")

        if depth < 0 then (ways, minCost)
        else if position > target then (ways, minCost)
        else if position == target then
          (ways + way, (minCost, Some(way.sum)).min)
        else if way.sum >= minCost.getOrElse(Long.MaxValue) then (ways, minCost)
        else if visited.contains(way) then (ways, minCost)
        else
          visited += way
          val (waysA, minCostA) =
            loop(position + A.diff, way + A.cost, ways, minCost, depth - 1)
          val (waysB, minCostB) =
            loop(position + B.diff, way + B.cost, ways, minCost, depth - 1)
          (waysA ++ waysB, (minCostA, minCostB).min)

      val result = loop(Position(0, 0), MultiSet.empty, Set.empty, None, depth)
      logger.debug(s"result: ${result}")
      result._2

    import breeze.linalg.*

    def solve: Option[(Long, Long)] =
      def isWhole(n: Double): Boolean    = BigDecimal(n)
        .setScale(3, BigDecimal.RoundingMode.HALF_UP).isWhole()
      def isPositive(n: Double): Boolean = n > 0

      val coefficients = DenseMatrix(
        (A.diff.x.toDouble, B.diff.x.toDouble),
        (A.diff.y.toDouble, B.diff.y.toDouble),
      )
      val constants    = DenseVector(target.x.toDouble, target.y.toDouble)
      val solution     = coefficients \ constants
      assert(solution.length == 2)
      if solution.forall(n => isWhole(n) && isPositive(n)) then
        Some((solution(0).round, solution(1).round))
      else None

  extension(minCost: (Option[Long], Option[Long]))

    def min: Option[Long] = minCost match
      case (Some(a), Some(b)) => Some(math.min(a, b))
      case (Some(a), None)    => Some(a)
      case (None, Some(b))    => Some(b)
      case _                  => None

  object Parser:
    import scala.util.matching

    // Button A: X+65, Y+27!Button B: X+32, Y+70!Prize: X=305, Y=4371!
    val parser: matching.Regex =
      """Button A\: X\+(\d+), Y\+(\d+)!Button B\: X\+(\d+), Y\+(\d+)!Prize\: X\=(\d+), Y\=(\d+)!"""
        .r

    def parse(
      line: String,
      errorCorrection: Long,
    ): (Position, Position, Position) = line match
      case parser(ax, ay, bx, by, tx, ty) => (
          Position(ax.toLong, ay.toLong),
          Position(bx.toLong, by.toLong),
          Position(tx.toLong + errorCorrection, ty.toLong + errorCorrection),
        )
      case _ => throw new RuntimeException("Unexpected case")

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String, errorCorrection: Long = 0L): Set[ClawMachine] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val lines = source.getLines().grouped(4).map(_.mkString("!")).toSet
      lines.map { line =>
        logger.debug(s"line: ${line}")
        val (a, b, target) = Parser.parse(line, errorCorrection)
        ClawMachine(Button(a, 3), Button(b, 1), target)
      }
    finally source.close()

  extension(machines: Set[ClawMachine])
    def cheapestWaysToWin(depth: Int): Long = machines.toList
      .flatMap(_.cheapestWayToWin(depth)).sum

  /** @return the fewest tokens to win all prices */
  def part1(machines: Set[ClawMachine]): Long =
    require(machines.nonEmpty, "machines.nonEmpty")
    logger.debug(s"machines: ${machines}")

    machines.cheapestWaysToWin(200)

  /** @return the fewest tokens to win all prices */
  def part2(machines: Set[ClawMachine]): Long =
    require(machines.nonEmpty, "machines.nonEmpty")
    logger.debug(s"machines: ${machines}")

    machines.flatMap(_.solve).map { case (a, b) => (a * 3) + (b * 1) }.sum
