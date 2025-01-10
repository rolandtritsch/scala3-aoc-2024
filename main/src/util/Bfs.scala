package util

/** Trait for breadth first search. */
trait Bfs:

  import scala.collection.mutable
  import util.Position
  import util.DPosition

  val logger: com.typesafe.scalalogging.Logger

  val start: Option[Position]
  val end: Option[Position]
  val blocked: Set[Position]
  def adjacent(p: Position, v: Set[Position] = Set.empty): Set[DPosition]

  /** @return
    *   the first path from start to end (or None if no path exists)
    *
    * @note
    *   Bfs ensures that the first path found is the shortest.
    */
  def findFirstIterative(): Option[Path] =
    val paths = mutable.Queue[(Position, Path)]()
    val visited = mutable.Set[Position]()

    if start.isEmpty || end.isEmpty then return None // scalafix:ok

    paths.enqueue((start.get, List(start.get))) // scalafix:ok
    visited.add(start.get) // scalafix:ok

    while paths.nonEmpty do
      val (current, path) = paths.dequeue()

      if current == end.get then return Some(path) // scalafix:ok
      else
        // format: off
        for
          n <- adjacent(current)
          next = n.toPosition
          if !visited.contains(next)
        do
          visited.add(next)
          paths.enqueue((next, path :+ next))
        end for
        // format: on
      end if
    end while

    None
  end findFirstIterative

  /** @return
    *   the shortest path from start to end (or None if no path exists)
    *
    * @note
    *   This is the recursive version of bfs.
    */
  @scala.annotation.tailrec
  final def findFirstRecursive(paths: Set[Path], visited: Set[Position]): Option[Path] =
    logger.debug(s"paths: ${paths}")
    val foundOne = paths.find(_.last == end.get) // scalafix:ok
    if foundOne.nonEmpty then foundOne
    else
      val (nextPaths, nexts) = paths.foldLeft((Set.empty[Option[Path]], Set.empty[Position])):
        case ((nextPaths, nexts), p) =>
          val nss = adjacent(p.last, visited).map(_.toPosition)
          val nps = if nss.isEmpty then None else nss.map(n => Some(p :+ n))
          (nextPaths ++ nps, nexts ++ nss)

      if nextPaths.isEmpty then None else findFirstRecursive(nextPaths.flatten, visited ++ nexts)
    end if
  end findFirstRecursive

end Bfs
