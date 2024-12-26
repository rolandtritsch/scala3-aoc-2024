package util

/** Trait for breadth first search. 
  * 
  * Makes sure it does not loop.
  */
sealed trait Bfs {
  val end: Position

  /** @return the first path found from the start to the end */
  def findFirst(current: Position, path: List[Position]): List[Position] = {
    ???
  }
}