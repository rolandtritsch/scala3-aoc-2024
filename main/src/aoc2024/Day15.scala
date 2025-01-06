package aoc2024

/** Day15 - Warehouse Woes
  *
  * @see https://adventofcode.com/2024/day/15
  *
  * OK. We have a warehouse. With boxes in it and walls and a robot.
  * The robot can move around and push boxes. The robot can't move
  * through walls or boxes. The robot needs to move around and push
  * boxes until it reaches the end of the List of Moves it was given
  *
  * We can model this in a couple of ways ...
  *
  * - Clearly we need a Position class to represent the positions
  *   of the robot and the boxes and the walls.
  * - We need a Move class to represent the moves the robot needs to
  *   make.
  * - We need a Warehouse class to represent the warehouse and the
  *   robot and the boxes and the walls.
  * - Note: The Robot has no direction it (just) moves.
  * - Note: We do not need to keep the dimensions of the warehouse,
  *   because the warehouse has an outer wall and the robot can't
  *   move through walls.
  * - Note: I was wondering, if I want to make the moves part of
  *   the Warehouse class or the Robot class, but I feel it is cleaner
  *   to keep the moves separate and create a new abstraction called
  *   a State, which is a tuple of a Warehouse and Moves.
  *
  * One problem to solve while solving this problem is to push boxes,
  * means if the next position is blocked by a box and there is space
  * between the box and the wall, the box gets moved to that free space
  * (which makes the next position available for the robot to move to).
  *
  * When implementing this with a 2-dimensional array, we get the concept
  * of position-is-free/unoccupied for free (pun intended). When using
  * list(s) of positions we need to (explicitly) model the free-space
  * as a list.
  *
  * Then we can ...
  *
  * - look for free-space (between the current position and the wall in
  *   the direction of the movement)
  * - move the box to that free-space, by ...
  *   - removing the free position from the free-space list
  *   - adding the free position to the boxes list
  *   - removing the box position from the boxes list
  *   - adding the box position to the free-space list
  * - move the robot to the (now free) next position 
  * 
  * part1:
  *
  * - We need to read in the warehouse and the moves.
  * - We need to execute on all moves. Every move will create a new
  *   State (Warehouse, Moves).
  * - When we are out of Moves, we need to sum up the GPS coordinates
  *   of all the boxes.
  */

object Day15:
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  /** A case class to represent a Position */
  case class Position(x: Int, y: Int):
    def gpsCoordinate: Int = (x * 100) + y
    def next(move: Move): Position = move match
      case Move.Up => Position(x - 1, y)
      case Move.Down => Position(x + 1, y)
      case Move.Left => Position(x, y - 1)
      case Move.Right => Position(x, y + 1)
    def isBlockedByWall(walls: Set[Position]): Boolean = walls.contains(this)
    def isBlockedByBox(boxes: Set[Position]): Boolean = boxes.contains(this)

  object Position:
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
  import scala.math.Ordering.Implicits._

  /** An enumeration to represent the possible moves */
  enum Move:
    case Up, Down, Left, Right

  /** A class to represent the Warehouse */
  class Warehouse(val robot: Position, val boxes: Set[Position], val spaces: Set[Position], val walls: Set[Position]):
    def this() = this(Position(0, 0), Set.empty, Set.empty, Set.empty)

    def clone(
      robot: Position = this.robot,
      boxes: Set[Position] = this.boxes,
      spaces: Set[Position] = this.spaces,
      walls: Set[Position] = this.walls
    ): Warehouse =
      Warehouse(robot, boxes, spaces, walls)

    /** @return the next Warehouse after the given Move */
    def move(move: Move): Warehouse =
      logger.debug(s"move: ${move}, this: ${this}")

      val nextRobot = robot.next(move)
      if (nextRobot.isBlockedByWall(walls)) this
      else if (nextRobot.isBlockedByBox(boxes))
        val (nextSpaces, nextBoxes, movedIt) =
          spaces.push(nextRobot, nextRobot, boxes, walls, move)
        logger.debug(s"nextSpaces: ${nextSpaces}, nextBoxes: ${nextBoxes}, movedIt: ${movedIt}")
        if (movedIt)
          val (finalSpaces, nextRobot) = nextSpaces.move(robot, move)
          this.clone(robot = nextRobot, boxes = nextBoxes, spaces = finalSpaces)
        else this
      else this.clone(robot = nextRobot)

    override def toString(): String =
      s"Warehouse(robot: ${robot}, boxes: ${boxes}, walls: ${walls})"

    def toStringPretty(): String =
      val Position(maxX, maxY) = walls.toList.max
      val wh =
        (0 to maxX).map { x =>
          (0 to maxY).map { y => {
            val pos = Position(x, y)
            if (robot == pos) '@'
            else if (boxes.contains(pos)) 'O'
            else if (walls.contains(pos)) '#'
            else '.'
          }
        }.mkString
        }.mkString("\n")
      s"\n${wh}\n"

  extension (spaces: Set[Position])
    /** @return the next Position/Spaces after the given Move */
    def move(robot: Position, move: Move): (Set[Position], Position) =
      (spaces - robot.next(move) + robot, robot.next(move))

    /** @return update Spaces/Boxes after the box was moved (if possible) */
    def push(box: Position, currentBox: Position, boxes: Set[Position], walls: Set[Position], move: Move): (Set[Position], Set[Position], Boolean) =
      logger.debug(s"box: ${box}, currentBox: ${currentBox}, move: ${move}, boxes: ${boxes}, spaces: ${spaces}, walls: ${walls}")

      if (currentBox.next(move).isBlockedByWall(walls)) (spaces, boxes, false)
      else if (currentBox.next(move).isBlockedByBox(boxes)) spaces.push(box, currentBox.next(move), boxes, walls, move)
      else (spaces - currentBox.next(move) + box, boxes + currentBox.next(move) - box, true)

  type State = (Warehouse, List[Move])

  extension (state: State)
    def next: State =
      logger.debug(s"state: ${state}")
      
      val (wh, moves) = state
      (wh.move(moves.head), moves.tail)

    def finalState: State = state match
      case (_, Nil) => state
      case _ => state.next.finalState

  /** @return the Warehouse from the given file */ 
  def readFileWarehouse(filename: String): Warehouse =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      source.getLines().toSeq.zipWithIndex.foldLeft(new Warehouse()) { case (wh, (line, x)) => {
        logger.debug(s"line: ${line}")
        line.zipWithIndex.foldLeft(wh) { case (wh, (c, y)) => c match {
          case '#' => wh.clone(walls = wh.walls + Position(x, y))
          case 'O' => wh.clone(boxes = wh.boxes + Position(x, y))
          case '.' => wh.clone(spaces = wh.spaces + Position(x, y))
          case '@' => wh.clone(robot = Position(x, y))
          case _ => throw new RuntimeException(s"Unexpected case")
        }}
      }}
    finally
      source.close()

  /** @return the Moves from the given file */ 
  def readFileMoves(filename: String): List[Move] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try
      val moves = source.getLines().mkString
      moves.map { case c => c match {
        case '^' => Move.Up
        case 'v' => Move.Down
        case '<' => Move.Left
        case '>' => Move.Right
        case _ => throw new RuntimeException(s"Unexpected case")
      }}.toList
    finally
      source.close()

  /** @return the sum of all GPS coordinates */
  def part1(state: State): Int =
    require(state._2.nonEmpty, "state._2.nonEmpty")
    logger.debug(s"state: ${state}")

    val (wh, _) = state.finalState
    wh.boxes.toList.map(_.gpsCoordinate).sum

  /** @return the solution for part2 */
  def part2(state: State): Int =
    require(state._2.nonEmpty, "state._2.nonEmpty")
    logger.debug(s"state: ${state}")

    state._1.robot.x
