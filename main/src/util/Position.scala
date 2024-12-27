package util

case class Position(x: Int, y: Int) {
  def next(obstacles: Set[Position], visited: Set[Position] = Set.empty): List[Position] = {
    List(
      Position(x - 1, y),
      Position(x + 1, y),
      Position(x, y - 1),
      Position(x, y + 1)
    ).filter(p => !obstacles.contains(p) && !visited.contains(p)).sorted
  }
}

object Position {
  implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
}

extension (source: scala.io.BufferedSource) {
  def start(token: Char = 'S'): Option[Position] = {
    val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

    try {
      source.getLines().zipWithIndex.foldLeft(Option.empty[Position]) { case (position, (line, x)) => {
        logger.debug(s"line: ${line}")

        line.zipWithIndex.foldLeft(position) { case (position, (c, y)) => c match {
          case `token` => Some(Position(x, y))
          case _ => position
        }}
      }}
    } finally {
      source.close()
    }
  }
}

extension (path: Option[List[Position]]) {
  def min(thatPath: Option[List[Position]])(using score: List[Position] => Int): Option[List[Position]] = (path, thatPath) match {
    case (None, None) => None
    case (Some(_), None) => path
    case (None, Some(_)) => thatPath
    case (Some(p), Some(tp)) => if (score(p) < score(tp)) path else thatPath
  }
}
