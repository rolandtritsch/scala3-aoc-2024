package util

/** A position (with a direction) in the 2-dimensional space */
case class DPosition(x: Int, y: Int, direction: DPosition.Direction):
  def toPosition: Position = Position(x, y)

object DPosition:

  enum Direction:
    case Up, Down, Left, Right

end DPosition
