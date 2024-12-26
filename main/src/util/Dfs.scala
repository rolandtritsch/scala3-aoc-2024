package util

/** Trait for depth first search. 
  * 
  * Makes sure it does not loop.
  */
trait Dfs {
  val logger: com.typesafe.scalalogging.Logger

  val end: Position
  val obstacles: Set[Position]

  /** @return 
    *   the first path found from the start to the end or None 
    * 
    * @note 
    *   The implementation is recursive, but not tail-recursive.
    *   Means it will run out of stack (fast). And it is slow. 
    */
  def findFirst(current: Position, path: List[Position] = List.empty): Option[List[Position]] = {
    logger.info(s"current: ${current}, path: ${path}")

    if (current == end) Some(path :+ current)
    else if (path.contains(current)) None
    else current.next(obstacles).foldLeft(Option.empty[List[Position]]) { case (p, n) => p match {
      case Some(_) => p
      case None => findFirst(n, path :+ current)
    }}
  }
}