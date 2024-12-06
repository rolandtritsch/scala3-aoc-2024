package aoc2024

/** Day06 - Guard Gallivant
  *
  * part1:
  *
  * Let's try something new. Let's use classes (and not functions
  * on types). Let's build and use a Grid that will behave like
  * a state-machine or an iterator. Means you can create it and
  * then run it and when it is done you can ask it for the number
  * of steps it took.
  *
  * The public methods will be something along the lines of ...
  *
  * - ctor: grid, initial guard position
  * - walk: the guard to walk the grid until it leaves the grid
  * - visted: shows the distunct number of visited positions
  *
  * The private methods to walk the grid will be along the lines of ...
  *
  * - step: take one step forward
  * - done: check, if Elvis has left the building
  *
  * part2:
  *
  * Let's brute-force this one.
  *
  * - add a new obstruction
  * - walk the lab
  * - check, if the walk ended up in a loop
  */

object Day06 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  enum State {
    case FREE, BLOCKED, OUTOFBOUNDS
  }
  import State._

  enum Direction {
    case UP, DOWN, LEFT, RIGHT
  }
  import Direction._

  case class Position(val row: Int, val col: Int, direction: Direction = UP)

  class Grid(val grid: Array[Array[State]], var guard: Position) {
    val rows = grid.size
    val cols = grid(0).size
    var steps = 0
    var done = false
    var looped = false
    val visited = scala.collection.mutable.Set.empty[Position]

    private def nextPosition: State = {
      val Position(row, col, direction) = guard
      direction match {
        case UP if(row - 1 < 0) => OUTOFBOUNDS
        case DOWN if(row + 1 >= rows) => OUTOFBOUNDS
        case LEFT if(col - 1 < 0) => OUTOFBOUNDS
        case RIGHT if(col + 1 >= cols) => OUTOFBOUNDS
        case UP => grid(row - 1)(col)
        case DOWN => grid(row + 1)(col)
        case LEFT => grid(row)(col - 1)
        case RIGHT => grid(row)(col + 1)
      }
    }

    private def step: Unit = {
      if(visited.contains(guard)) {
        done = true
        looped = true
      } else {
        val Position(row, col, direction) = guard
        visited.add(guard)
        guard = direction match {
          case UP if(nextPosition == FREE) => {steps += 1; Position(row - 1, col, UP)}
          case UP if(nextPosition == BLOCKED) => {Position(row, col, RIGHT)}
          case DOWN if(nextPosition == FREE) => {steps += 1; Position(row + 1, col, DOWN)}
          case DOWN if(nextPosition == BLOCKED) => {Position (row, col, LEFT)}
          case LEFT if(nextPosition == FREE) => {steps += 1; Position(row, col - 1, LEFT)}
          case LEFT if(nextPosition == BLOCKED) => {Position(row, col, UP)}
          case RIGHT if(nextPosition == FREE) => {steps += 1; Position(row, col + 1, RIGHT)}
          case RIGHT if(nextPosition == BLOCKED) => {Position(row, col, DOWN)}
          case _ if(nextPosition == OUTOFBOUNDS) => {
            steps += 1
            done = true
            guard
          }
          case _ => throw new RuntimeException("Unexpected case")
        }
      }
    }
        
    def walk: Unit = {
      while(!done) step
    }
  }

  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Grid = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    var guard = Position(0, 0)
    val source = Source.fromFile(filename)
    val grid = try {
      source.getLines().zipWithIndex.map { (line, row) =>
        logger.debug(s"line: ${line}")
        val parsed = line.zipWithIndex.map { (c, col) => c match {
          case '.' => FREE
          case '#' => BLOCKED
          case '^' => {
            guard = Position(row, col)
            FREE
          }
        }}.toArray
        parsed
      }.toArray
    } finally {
      source.close()
    }
    Grid(grid, guard)
  }

  private def findLoops(lab: Grid): Int = {
    var counter = 0
    (0 until lab.rows).map { row => {
      (0 until lab.cols).map { col => {
        val grid = clone(lab, row, col, BLOCKED) 
        val lookForLoop = Grid(grid, lab.guard)
        lookForLoop.walk
        if(lookForLoop.looped) counter += 1
      }}
    }}
    counter
  }

  private def clone(lab: Grid, labRow: Int, labCol: Int, state: State): Array[Array[State]] = {
    (0 until lab.rows).map { row => {
      (0 until lab.cols).map { col => {
        if (row == labRow && col == labCol) state else lab.grid(row)(col)
      }}.toArray
    }}.toArray
  }

  /** @return the number of positions covered while walking the grid */
  def part1(lab: Grid): Int = {
    require(!lab.done, "!lab.done")
    logger.debug(s"lab: ${lab}")

    lab.walk
    assert(!lab.looped)
    lab.visited.map { case Position(row, col, _) => (row, col) }.size
  }

  /** @return the solution for part2 */
  def part2(lab: Grid): Int = {
    require(!lab.done, "!lab.done")
    logger.debug(s"lab: ${lab}")

    findLoops(lab)
  }
}
