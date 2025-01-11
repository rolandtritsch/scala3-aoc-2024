package util

/** A position in the 2-dimensional space */
case class Position(x: Int, y: Int):
  def toDPosition: DPosition = DPosition(x, y, DPosition.Direction.Up)

  def adjacent: Set[Position] = Set(up, down, left, right)

  def up = Position(x - 1, y)
  def down = Position(x + 1, y)
  def left = Position(x, y - 1)
  def right = Position(x, y + 1)

end Position
