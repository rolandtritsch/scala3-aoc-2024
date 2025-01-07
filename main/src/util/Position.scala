package util

/** A position in the 2-dimensional space */
case class Position(x: Int, y: Int)

object Position:
    implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))
