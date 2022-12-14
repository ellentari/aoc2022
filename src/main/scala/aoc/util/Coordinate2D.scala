package aoc.util

case class Coordinate2D(x: Int, y: Int) {

  def up: Coordinate2D = copy(y = y + 1)
  def down: Coordinate2D = copy(y = y - 1)
  def left: Coordinate2D = copy(x = x - 1)
  def right: Coordinate2D = copy(x = x + 1)

  def downLeft: Coordinate2D = Coordinate2D(x = x - 1, y = y - 1)
  def downRight: Coordinate2D = Coordinate2D(x = x + 1, y = y - 1)

  def upLeft: Coordinate2D = Coordinate2D(x = x - 1, y = y + 1)
  def upRight: Coordinate2D = Coordinate2D(x = x + 1, y = y + 1)

  def adjacent4: List[Coordinate2D] = List(up, right, down, left)
  def adjacent8: List[Coordinate2D] = List(up, upRight, right, downRight, down, downLeft, left, upLeft)

}

object Coordinate2D {
  val Zero: Coordinate2D = Coordinate2D(0, 0)

  def parse(s: String): Coordinate2D = {
    val parts = s.split(",")
    Coordinate2D(parts(0).trim.toInt, parts(1).trim.toInt)
  }
}
