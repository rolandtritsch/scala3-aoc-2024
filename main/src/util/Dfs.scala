package util

/** Trait for depth first search. 
  * 
  * Makes sure it does not loop.
  */
trait Dfs {
  import scala.collection.mutable

  val logger: com.typesafe.scalalogging.Logger

  val end: Position
  val obstacles: Set[Position]

  /** @return 
    *   the first path found from the start to the end or None 
    * 
    * @note 
    *   The implementation is recursive, but not tail-recursive.
    *   Means it might run out of stack (depending on the size and 
    *   shape of the grid). And it is slow. 
    */
  def findFirst(current: Position, path: List[Position] = List.empty): Option[List[Position]] = {
    require(!obstacles.contains(current), s"obstacles.contains(${current}) - path: ${path}")
    logger.debug(s"current: ${current}, path: ${path}")

    if (current == end) Some(path :+ current)
    else if (path.contains(current)) None
    else current.next(obstacles).foldLeft(Option.empty[List[Position]]) { case (p, n) => p match {
      case Some(_) => p
      case None => findFirst(n, path :+ current)
    }}
  }

  /** A global map of visited positions and the shortest path found so far.
    * We need this to cut off the search as soon as we find a path with
    * a higher score than the one we have found so far.
    */
  given mutable.Map[Position, Int] = mutable.Map.empty[Position, Int].withDefaultValue(Int.MaxValue)

  /** @return
    *   the cheapest path found from the start to the end or None
    * 
    * @note
    *   Cheap is defined by the scoring function. A lower score means
    *   a better path. The default scoring function is the lenth of
    *   the path. Means the default is the shortest path.
    * 
    * @note 
    *   The implementation is recursive, but not tail-recursive.
    *   Means it will run out of stack (fast). And it is slow. 
    */
  def findCheapest(start: Position): Option[List[Position]] = {
    def findCheapest(
      current: Position, 
      path: List[Position],
      bestPath: Option[List[Position]]
    )(
      using visited: mutable.Map[Position, Int]
    )(
      using score: List[Position] => Int
    ): Option[List[Position]] = {
      require(!obstacles.contains(current), s"obstacles.contains(${current}) - path: ${path}")
      logger.debug(s"current: ${current}, path: ${path}")

      if (current == end) Some(path :+ current).min(bestPath)
      else if (score(path :+ current) >= visited(current)) bestPath
      else current.next(obstacles).foldLeft(bestPath) { case (bp, n) => {
        visited.update(current, score(path :+ current))
        findCheapest(n, path :+ current, bp).min(bp)
      }}
    }

    findCheapest(start, List.empty, None)
  }

  given (List[Position] => Int) = {
    p => p.size
  }

  extension (path: Option[List[Position]]) {
    def min(thatPath: Option[List[Position]])(using score: List[Position] => Int): Option[List[Position]] = (path, thatPath) match {
      case (None, None) => None
      case (Some(_), None) => path
      case (None, Some(_)) => thatPath
      case (Some(p), Some(tp)) => if (score(p) < score(tp)) path else thatPath
    }
  }
}