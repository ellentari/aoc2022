package aoc.util

case class Coordinate3D(x: Int, y: Int, z: Int) {

  def adjacent6: List[Coordinate3D] = List(
    copy(x = x + 1),
    copy(x = x - 1),
    copy(y = y + 1),
    copy(y = y - 1),
    copy(z = z + 1),
    copy(z = z - 1)
  )

  def adjacent26: List[Coordinate3D] =
    for {
      xi <- List(-1, 0, 1)
      yi <- List(-1, 0, 1)
      zi <- List(-1, 0, 1) if (xi, yi, zi) != (0, 0, 0)
    } yield Coordinate3D(x + xi, y + yi, z + zi)

}

object Coordinate3D {
  val Zero: Coordinate3D = Coordinate3D(0, 0, 0)

  implicit val orderingCoordinate3D: Ordering[Coordinate3D] = Ordering.by(c => (c.x, c.y, c.z))
}



