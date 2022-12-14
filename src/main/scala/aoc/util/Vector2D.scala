package aoc.util

abstract case class Vector2D private(left: Coordinate2D, right: Coordinate2D) {

  def minX: Int = left.x
  def minY: Int = right.y min left.y

  def maxX: Int = right.x
  def maxY: Int = right.y max left.y

  def coordinates: Iterable[Coordinate2D] =
    if (isHorizontal || isVertical)
      ys.flatMap(y => xs.map(Coordinate2D(_, y)))
    else
      xs.zip(ys).map((Coordinate2D.apply _).tupled)

  private def isHorizontal: Boolean = left.y == right.y
  private def isVertical: Boolean = left.x == right.x

  private def xs: Range = left.x to right.x
  private def ys: Range = {
    val delta = if (left.y < right.y) 1 else -1
    left.y to right.y by delta
  }
}

object Vector2D {
  def from(c1: Coordinate2D, c2: Coordinate2D): Vector2D = {
    val left = if (c1.x <= c2.x) c1 else c2
    val right = if (c1.x > c2.x) c1 else c2
    new Vector2D(left, right) {}
  }
}
