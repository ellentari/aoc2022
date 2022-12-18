package aoc

import aoc.algo.BFS
import aoc.util.Coordinate3D

object Day18 extends App {

  case class Cube1x1(coordinate: Coordinate3D) {
    def adjacent: List[Cube1x1] = coordinate.adjacent6.map(Cube1x1)
  }

  def solvePart1(cubes: Set[Cube1x1]): Int = calculateOuterSurface(cubes)

  def solvePart2(cubes: Set[Cube1x1]): Int = {
    val emptySpace = calculateEmptySpace(cubes)

    val reachable = BFS.discoverRegion(emptySpace.minBy(_.coordinate))(
      _.adjacent.filter(emptySpace.contains))

    val unreachable = emptySpace -- reachable

    val outer = calculateOuterSurface(cubes)
    val inner = calculateOuterSurface(unreachable)

    outer - inner
  }

  private def calculateOuterSurface(cubes: Set[Cube1x1]): Int =
    cubes.view.map(c => 6 - c.adjacent.count(cubes.contains)).sum

  private def calculateEmptySpace(cubes: Set[Cube1x1]) = {
    val minX = cubes.map(_.coordinate.x).min
    val maxX = cubes.map(_.coordinate.x).max

    val minY = cubes.map(_.coordinate.y).min
    val maxY = cubes.map(_.coordinate.y).max

    val minZ = cubes.map(_.coordinate.z).min
    val maxZ = cubes.map(_.coordinate.z).max

    (for {
      x <- minX - 1 to maxX + 1
      y <- minY - 1 to maxY + 1
      z <- minZ - 1 to maxZ + 1
      cube = Cube1x1(Coordinate3D(x, y, z)) if !cubes.contains(cube)
    } yield cube).toSet
  }

  private def parseCube(raw: String) = raw match {
    case s"$x,$y,$z" => Cube1x1(Coordinate3D(x.toInt, y.toInt, z.toInt))
  }

  private val sample = Input.asList("day18_sample.txt").map(parseCube).toSet
  private val input = Input.asList("day18.txt").map(parseCube).toSet

  println(solvePart1(sample)) // 64
  println(solvePart1(input)) // 4308

  println(solvePart2(sample)) // 58
  println(solvePart2(input)) // 2540

}
