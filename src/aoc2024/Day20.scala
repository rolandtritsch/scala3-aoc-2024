package aoc2024

/** Day20 - Race Condition
  * 
  * Here we go again ... another grid traversal/walking problem.
  * But with a twist: We are going to create variations of grids
  * to find the "best" one.
  * 
  * The (initial) datastructures will will look like the datastructures
  * from Day16.
  * 
  * Part1:
  *
  * - Read in the race track and the program
  * - Find the shortest path (in picoseconds)
  * - Replace two walls with spaces
  *   - We do not want to remove outer walls
  *   - We (probably) do not need to keep track of the free space (we
  *     can just remove the wall)
  * - Find the shortest path (in picoseconds) ... again
  * - Rinse and repeat
  * 
  * Part1 (revisted):
  * 
  * - Obviously it's not that easy ...
  * - Thought that I can (just) replace the wall with a space and
  *   that would be it
  * - But that is not the case. Instead I need to walk the shortest path
  *   and and on that shortest need to replace a wall with a space (in
  *   directionof travel), if the position after the wallcis free and then 
  *   walk the race track with that wall removed
  * - Hhhmmm ...
  */

object Day20 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)
  
  case class Position(x: Int, y: Int) {
    def next(walls: Set[Position]): Seq[Position] = {
      Seq(
        Position(x - 1, y),
        Position(x + 1, y),
        Position(x, y - 1),
        Position(x, y + 1)
      ).filter(!walls.contains(_))
    }
    
    /** @return the shortest path through the race track */
    def dfs(
      end: Position,
      walls: Set[Position], 
      path: List[Position] = List.empty, 
      shortestPath: Option[List[Position]] = None, 
      visited: Map[Position, Int] = Map.empty.withDefaultValue(Int.MaxValue)
    ): (Map[Position, Int], Option[List[Position]]) = {
      if (this == end) (visited, shortestPath.min(path))
      else if (path.size >= visited(this)) (visited, None)
      else {
        next(walls).foldLeft((visited.updated(this, path.size), shortestPath)) { case ((v, sp), n) => {
          val (nextv, nextsp) = n.dfs(end, walls, path :+ this, sp, v)
          if (nextsp.isEmpty) (nextv, sp) else (nextv, nextsp)
        }}
      }    
    }

    extension (shortestPath: Option[List[Position]]) {
      def min(path: List[Position]): Option[List[Position]] = shortestPath match {
        case Some(sp) if (sp.size > path.size) => Some(path)
        case Some(sp) => Some(sp)   
        case None => Some(path)
      }
    }
  }

  object Position {
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
  }
  import scala.math.Ordering.Implicits._

  class RaceTrack(val walls: Set[Position], val end: Position) {
    def this() = this(Set.empty, Position(0, 0))
    def clone(
      walls: Set[Position] = this.walls,
      end: Position = this.end
    ): RaceTrack = {
      RaceTrack(walls, end)
    }

    def cheats(shortestPath: Set[Position]): Set[Position] = {
      val Position(x, y) = walls.max
      val (maxX, maxY) = (x + 1, y + 1)
      val horizontals = 
        (1 to maxX - 2).flatMap { x => {
          (1 to maxY - 4).map { y => {
            (Position(x, y), Position(x, y + 1), Position(x, y + 2))
          }}
        }}.toSet
      val verticals = 
        (1 to maxY - 2).flatMap { y => {
          (1 to maxX - 4).map { x => {
            (Position(x, y), Position(x + 1, y), Position(x + 2, y))
          }}
        }}.toSet
      (horizontals ++ verticals).filter { case (p1, p2, p3) => {
        !walls.contains(p1) && walls.contains(p2) && !walls.contains(p3) 
        && (shortestPath.contains(p1) || shortestPath.contains(p3))
      }}.map(_._2)
    }

    def shortCuts(program: Position, shortestPath: List[Position]): Set[(Position, Option[List[Position]])] = {
      cheats(shortestPath.toSet).map { c => {
        val cheatingWalls = walls - c
        (c, program.dfs(end, cheatingWalls)._2)
      }}
    }
  }

  type State = (RaceTrack, Position)

  /** @return the RaceTrack and the Program (in its starting position) */ 
  def readFile(filename: String): State = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      val (track, program) = source.getLines().toSeq.zipWithIndex.foldLeft((new RaceTrack(), Option.empty[Position])) { case (state, (line, x)) => {
        logger.debug(s"line: ${line}")
        line.zipWithIndex.foldLeft(state) { case ((track, program), (c, y)) => c match {
          case '#' => (track.clone(walls = track.walls + Position(x, y)), program)
          case 'S' => (track, Some(Position(x, y)))
          case 'E' => (track.clone(end = Position(x, y)), program)
          case '.' => (track, program)
          case _ => throw new RuntimeException(s"Unexpected case")
        }}
      }}

      (track, program.get)
    } finally {
      source.close()
    }
  }

  /** @return the number of short-cuts that save more than 100 picoseconds */
  def part1(state: State, threshold: Int = 100): Int = {
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")
 
    val (track, program) = state

    val (_, shortestPath) = program.dfs(track.end, track.walls)
    val shortestPathLength = shortestPath.get.length

    val shortCutsPathLength = track.shortCuts(program, shortestPath.get).map { case (c, p) => {
      (c, p.getOrElse(List.empty).length)
    }}.toList

    val shortCutsValue = shortCutsPathLength.map { (c, p) => {
      (c, shortestPathLength - p)
    }}

    shortCutsValue.groupBy(_._2).map { (cheatValue, cheatList) => {
      (cheatValue, cheatList.size)
    }}.count(_._1 >= threshold)
  }

  /** @return the solution for part2 */
  def part2(state: State): Int = {
    require(state._1.walls.nonEmpty, "state._1.walls.nonEmpty")
    logger.debug(s"state: ${state}")

    state._1.walls.size
  }
}
