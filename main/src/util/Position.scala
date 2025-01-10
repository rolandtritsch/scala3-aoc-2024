package util

trait P:
  val x: Int
  val y: Int
  def toPosition = Position(x, y)

/** A position in the 2-dimensional space */
case class Position(x: Int, y: Int) extends P

object Position:
  implicit val ordering: Ordering[Position] = Ordering.by(p => (p.x, p.y))

enum Direction: 
  case Up, Down, Left, Right

/** A position (with a direction) in the 2-dimensional space */
case class DPosition(x: Int, y: Int, direction: Direction) extends P
  
/** A relative position (with a relative direction) in the 2-dimensional space (for neighbors) */
case class RPosition(x: Int, y: Int, relative: Direction, from: Position) extends P
