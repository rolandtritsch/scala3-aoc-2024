package util

/** A position (with a direction) in the 2-dimensional space */
case class DPosition(x: Int, y: Int, direction: DPosition.Direction):
  def toPosition: Position = Position(x, y)

  def adjacent: Set[DPosition] = Set(up, down, left, right)

  def up = DPosition(x - 1, y, DPosition.Direction.Up)
  def down = DPosition(x + 1, y, DPosition.Direction.Down)
  def left = DPosition(x, y - 1, DPosition.Direction.Left)
  def right = DPosition(x, y + 1, DPosition.Direction.Right)

end DPosition

object DPosition:

  enum Direction:
    case Up, Down, Left, Right

end DPosition
