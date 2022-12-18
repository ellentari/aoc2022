package aoc

import aoc.algo.BFS
import aoc.util.Grid

object Day12 extends App {

  type Label = Char
  type Elevation = Char

  case class Cell(label: Label, elevation: Elevation)

  case class HeightMap(grid: Grid[Cell]) {

    private val start = grid.indexOf(_.label == 'S').get
    private val end = grid.indexOf(_.label == 'E').get

    def findShortestPathFromStartToEnd: Option[Int] =
      BFS.shortestPathLength(start)(
        from => grid.adjacent4(from).filter(canClimb(from, _)),
        _ == end)

    def findShortestPathFromEndToNearestStart: Option[Int] =
      BFS.shortestPathLength(end)(
        to => grid.adjacent4(to).filter(canClimb(_, to)),
        grid(_).elevation == 'a'
      )

    private def canClimb(from: Grid.Index, to: Grid.Index): Boolean =
      (grid(to).elevation - grid(from).elevation) <= 1

  }

  def solvePart1(map: HeightMap): Option[Int] = map.findShortestPathFromStartToEnd

  def solvePart2(map: HeightMap): Option[Int] = map.findShortestPathFromEndToNearestStart

  private def parseHeightMap(raw: String): HeightMap =
    HeightMap(Grid.parseCharacterGrid(raw).map {
      case 'S' => Cell('S', 'a')
      case 'E' => Cell('E', 'z')
      case height => Cell(height, height)
    })

  private val sample = parseHeightMap(Input.asString("day12_sample.txt"))
  private val input = parseHeightMap(Input.asString("day12.txt"))

  println(solvePart1(sample)) // 31
  println(solvePart1(input)) // 449

  println(solvePart2(sample)) // 29
  println(solvePart2(input)) // 443

}
