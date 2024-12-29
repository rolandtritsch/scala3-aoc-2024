package util

/** A path between two positions */
type Path = List[Position]

extension (path: Option[Path]) {
  def min(thatPath: Option[Path])(using score: Path => Int): Option[Path] = (path, thatPath) match {
    case (None, None) => None
    case (Some(_), None) => path
    case (None, Some(_)) => thatPath
    case (Some(p), Some(tp)) => if (score(p) < score(tp)) path else thatPath
  }
}