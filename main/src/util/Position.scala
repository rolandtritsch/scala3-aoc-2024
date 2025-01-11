package util

/** A position in the 2-dimensional space */
case class Position(x: Int, y: Int):
  def toDPosition: DPosition = DPosition(x, y, DPosition.Direction.Up)

  def adjacent: Set[Position] = Set(up, down, left, right)

  def up: Position = Position(x - 1, y)
  def down: Position = Position(x + 1, y)
  def left: Position = Position(x, y - 1)
  def right: Position = Position(x, y + 1)

end Position
