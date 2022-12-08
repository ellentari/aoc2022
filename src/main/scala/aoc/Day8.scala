package aoc

import aoc.util.Grid

object Day8 extends App {

  type TreeSize = Int

  def solvePart1(grid: Grid[TreeSize]): Int = {
    def isVisible(cc: (Int, Int)): Boolean =
      indicesIn4Directions(cc, grid)
        .exists(_.forall(grid(_) < grid(cc)))

    grid.rowColumnIndices.count(isVisible)
  }

  def solvePart2(grid: Grid[TreeSize]): Int = {
    def viewingDistance(thisTree: TreeSize)(otherTrees: IndexedSeq[TreeSize]) = {
      val distance = otherTrees.takeWhile(_ < thisTree).size
      if (distance == otherTrees.size) distance else distance + 1
    }

    def scenicScore(cc: (Int, Int)): Int =
      indicesIn4Directions(cc, grid)
        .map(_.map(grid(_)))
        .map(viewingDistance(grid(cc)))
        .product

    grid.rowColumnIndices.map(scenicScore).maxOption.getOrElse(0)
  }

  private def indicesIn4Directions(cc: (Int, Int), grid: Grid[TreeSize]) =
    List(
      grid.indicesAbove(cc),
      grid.indicesRight(cc),
      grid.indicesBelow(cc),
      grid.indicesLeft(cc)
    )

  private val sample = Grid.parseCharacterGrid(Input.asString("day8_sample.txt")).map(_ - '0')
  private val input = Grid.parseCharacterGrid(Input.asString("day8.txt")).map(_ - '0')

  println(solvePart1(sample)) // 21
  println(solvePart1(input)) // 1669

  println(solvePart2(sample)) // 8
  println(solvePart2(input)) // 331344

}
