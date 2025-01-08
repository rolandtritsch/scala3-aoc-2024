package util

/** Trait for depth first search. */
trait Dfs:
    import scala.collection.mutable

    val logger: com.typesafe.scalalogging.Logger

    val end: Option[Position]
    val blocked: Set[Position]
    def adjacent(p: Position, v: Set[Position] = Set.empty): Set[Position]

    /** @return
      *   the first path found from the start to the end or None
      *
      * @note
      *   The implementation is recursive, but not tail-recursive. Means it
      *   might run out of stack (depending on the size and shape of the grid).
      *   And it is slow.
      */
    def findFirst(current: Position, path: Path = List.empty): Option[Path] =
        require(
          !blocked.contains(current),
          s"blocked.contains(${current}) - path: ${path}",
        )
        logger.debug(s"current: ${current}, path: ${path}")

        if current == end.get then Some(path :+ current)
        else if path.contains(current) then None
        else
            adjacent(current).foldLeft(Option.empty[Path]): (p, n) =>
                p match
                    case Some(_) => p
                    case None    => findFirst(n, path :+ current)
        end if
    end findFirst

    /** A global map of visited positions and the shortest path found so far. We
      * need this to cut off the search as soon as we find a path with a higher
      * score than the one we have found so far.
      */
    given mutable.Map[Position, Int] = mutable.Map.empty[Position, Int]
        .withDefaultValue(Int.MaxValue)

    /** @return
      *   the cheapest path found from the start to the end or None
      *
      * @note
      *   Cheap is defined by the scoring function. A lower score means a better
      *   path. The default scoring function is the lenth of the path. Means the
      *   default is the shortest path.
      *
      * @note
      *   The implementation is recursive, but not tail-recursive. Means it will
      *   run out of stack (fast). And it is slow.
      */
    def findCheapest(start: Position): Option[Path] =
        def findCheapest(current: Position, path: Path, bestPath: Option[Path])(
            using visited: mutable.Map[Position, Int]
        )(using score: Path => Int): Option[Path] =
            require(
              !blocked.contains(current),
              s"blocked.contains(${current}) - path: ${path}",
            )
            logger.debug(s"current: ${current}, path: ${path}")

            if current == end.get then Some(path :+ current).min(bestPath)
            else if score(path :+ current) >= visited(current) then bestPath
            else
                adjacent(current).foldLeft(bestPath):(bp, n) =>
                    visited.update(current, score(path :+ current))
                    findCheapest(n, path :+ current, bp).min(bp)
            end if
        end findCheapest

        findCheapest(start, List.empty, None)
    end findCheapest

    given (Path => Int) = p => p.size
end Dfs
