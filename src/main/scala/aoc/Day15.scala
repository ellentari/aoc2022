package aoc

import aoc.util.{Coordinate2D, DisjointIntervalSet, Interval}

object Day15 extends App {

  case class ScannedArea(sensor: Coordinate2D, closestBeacon: Coordinate2D) {

    private val maxDistance = sensor.manhattanDistanceTo(closestBeacon)

    def contains(coordinate: Coordinate2D): Boolean =
      sensor.manhattanDistanceTo(coordinate) <= maxDistance

    def xIntervalAtY(y: Int): Option[Interval] = {
      val diffY = (sensor.y - y).abs
      val diffX = maxDistance - diffY

      Option.when(diffX >= 0)(Interval(sensor.x - diffX, sensor.x + diffX))
    }
  }

  def solvePart1(scannedAreas: List[ScannedArea], targetY: Int): Int = {
    val coveredXs = DisjointIntervalSet.from(scannedAreas.flatMap(_.xIntervalAtY(targetY)): _*)
    val coveredByBeacons = DisjointIntervalSet.from(scannedAreas
      .map(_.closestBeacon)
      .filter(_.y == targetY)
      .map(c => Interval(c.x, c.x)): _*)

    coveredXs
      .removeAll(coveredByBeacons)
      .size
  }

  def solvePart2(scannedAreas: List[ScannedArea], coordinateLimit: Int): Option[Long] = {
    val xInterval = Interval(0, coordinateLimit)
    val yInterval = Interval(-coordinateLimit, 0)

    (for {
      y <- yInterval.toRange.view
      coveredXs = DisjointIntervalSet.from(scannedAreas.flatMap(_.xIntervalAtY(y)): _*)
      x <- xInterval.removeAll(coveredXs)
    } yield x.toLong * 4_000_000 - y).headOption
  }

  private def parseScannedArea(raw: String): ScannedArea = raw match {
    case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" =>
      ScannedArea(Coordinate2D(x1.toInt, -y1.toInt), Coordinate2D(x2.toInt, -y2.toInt))
  }

  private val sample = Input.asList("day15_sample.txt").map(parseScannedArea)
  private val input = Input.asList("day15.txt").map(parseScannedArea)

  println(solvePart1(sample, targetY = -10)) // 26
  println(solvePart1(input, targetY = -2000000)) // 5256611

  println(solvePart2(sample, coordinateLimit = 20)) // 56000011
  println(solvePart2(input, coordinateLimit = 4000000)) // 13337919186981

}
