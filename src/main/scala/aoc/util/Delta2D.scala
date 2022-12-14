package aoc.util

case class Delta2D(dx: Int, dy: Int) {
  def multiply(by: Int): Delta2D = Delta2D(dx * by, dy * by)
  def applyTo(cc: Coordinate2D): Coordinate2D = Coordinate2D(cc.x + dx, cc.y + dy)
}

object Delta2D {
  val Zero: Delta2D = Delta2D(0, 0)
}


