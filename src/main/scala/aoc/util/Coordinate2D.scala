package aoc.util

case class Coordinate2D(x: Int, y: Int) {

  def addX(dx: Int): Coordinate2D = copy(x = x + dx)
  def addY(dy: Int): Coordinate2D = copy(y = y + dy)
  def add(delta: Delta2D): Coordinate2D = copy(x = x + delta.dx, y = y + delta.dy)
  def add(dx: Int, dy: Int): Coordinate2D = copy(x = x + dx, y = y + dy)

  def up: Coordinate2D = addY(1)
  def down: Coordinate2D = addY(-1)
  def left: Coordinate2D = addX(-1)
  def right: Coordinate2D = addX(1)

  def downLeft: Coordinate2D = Coordinate2D(x = x - 1, y = y - 1)
  def downRight: Coordinate2D = Coordinate2D(x = x + 1, y = y - 1)

  def upLeft: Coordinate2D = Coordinate2D(x = x - 1, y = y + 1)
  def upRight: Coordinate2D = Coordinate2D(x = x + 1, y = y + 1)

  def adjacent4: List[Coordinate2D] = List(up, right, down, left)
  def adjacent8: List[Coordinate2D] = List(up, upRight, right, downRight, down, downLeft, left, upLeft)

  def manhattanDistanceTo(to: Coordinate2D): Int = (x - to.x).abs + (y - to.y).abs

}

object Coordinate2D {
  val Zero: Coordinate2D = Coordinate2D(0, 0)

  implicit val orderingCoordinate3D: Ordering[Coordinate2D] = Ordering.by(c => (c.x, -c.y))

  def parse(s: String): Coordinate2D = {
    val parts = s.split(",")
    Coordinate2D(parts(0).trim.toInt, parts(1).trim.toInt)
  }
}
