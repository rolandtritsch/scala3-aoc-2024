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
    val paths = mutable.Queue[(Position, List[Position])]()
    val visited = mutable.Set[Position]()
    
    paths.enqueue((start, List(start)))
    visited.add(start)
    
    while (paths.nonEmpty) {
      val (current, path) = paths.dequeue()
      
      if (current == end) {
        return Some(path)
      } else {
        for {
          next <- current.next(obstacles)
          if !visited.contains(next)
        } {
          visited.add(next)
          paths.enqueue((next, path :+ next))
        }
      }
    }
    
    None
  }

  /** @return the shortest path from start to end (or None if no path exists)
    * 
    * @note This is the recursive version of bfs.
    */
  @scala.annotation.tailrec
  final def findFirstRecursive(paths: Set[List[Position]], visited: Set[Position]): Option[List[Position]] = {
    val foundOne = paths.find(_.last == end)
    if (foundOne.nonEmpty) foundOne
    else {
      val (nextPaths, nexts) = paths.foldLeft((Set.empty[Option[List[Position]]], Set.empty[Position])) { case ((nextPaths, nexts), p) => {
        val nss = p.last.next(obstacles, visited)
        val nps = if (nss.isEmpty) None
        else nss.map { n => {
          Some(p :+ n)
        }}
        (nextPaths ++ nps, nexts ++ nss)
      }}

      if (nextPaths.isEmpty) None
      else findFirstRecursive(nextPaths.flatten, visited ++ nexts)
    } 
  }
}