package util

/** Trait for breadth first search.
  */
trait Bfs {
  import scala.collection.mutable

  val logger: com.typesafe.scalalogging.Logger

  val end: Position
  val obstacles: Set[Position]

  /** @return the first path from start to end (or None if no path exists)
    * 
    * @note Bfs ensures that the first path found is the shortest.
    */
  def findFirstIterative(start: Position): Option[List[Position]] = {
    val bestPathSoFar = mutable.Queue[(Position, List[Position])]()
    val visited = mutable.Set[Position]()
    
    bestPathSoFar.enqueue((start, List(start)))
    visited.add(start)
    
    while (bestPathSoFar.nonEmpty) {
      val (current, path) = bestPathSoFar.dequeue()
      
      if (current == end) {
        return Some(path)
      } else {
        for {
          next <- current.next(obstacles)
          if !visited.contains(next)
        } {
          visited.add(next)
          bestPathSoFar.enqueue((next, path :+ next))
        }
      }
    }
    
    None
  }

  /** @return the shortest path from start to end (or None if no path exists)
    * 
    * @note This is the recursive version of bfs.
    */
  def findFirstRecursive(start: Position): Option[List[Position]] = {
    val paths = mutable.Queue[(Position, List[Position])]()
    
    def findFirst(current: Position, visited: Set[Position] = Set.empty): Option[List[Position]] = {
      if (paths.isEmpty) None
      else {
        val (current, path) = paths.dequeue()
        
        if (current == end) Some(path)
        else current.next(obstacles, visited).foldLeft(Option.empty[List[Position]]) { (bp, n) => {
          paths.enqueue((n, path :+ n))
          findFirst(n, visited + n).min(bp)
        }}
      }
    }
    
    paths.enqueue((start, List(start)))
    findFirst(start)
  }

  given (List[Position] => Int) = {
    p => p.size
  }
}