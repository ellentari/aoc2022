package aoc

import aoc.util.Grid
import aoc.util.Grid.Index

object Day8 extends App {

  type TreeSize = Int

  def solvePart1(grid: Grid[TreeSize]): Int = {
    def isVisible(index: Index): Boolean =
      indicesIn4Directions(index, grid)
        .exists(_.forall(grid(_) < grid(index)))

    grid.rowColumnIndices.count(isVisible)
  }

  def solvePart2(grid: Grid[TreeSize]): Int = {
    def viewingDistance(thisTree: TreeSize)(otherTrees: IndexedSeq[TreeSize]) = {
      val distance = otherTrees.takeWhile(_ < thisTree).size
      if (distance == otherTrees.size) distance else distance + 1
    }

    def scenicScore(index: Index): Int =
      indicesIn4Directions(index, grid)
        .map(_.map(grid(_)))
        .map(viewingDistance(grid(index)))
        .product

    grid.rowColumnIndices.map(scenicScore).maxOption.getOrElse(0)
  }

  private def indicesIn4Directions(index: Index, grid: Grid[TreeSize]) =
    List(
      grid.indicesAbove(index),
      grid.indicesRight(index),
      grid.indicesBelow(index),
      grid.indicesLeft(index)
    )

  private val sample = Grid.parseCharacterGrid(Input.asString("day8_sample.txt")).map(_ - '0')
  private val input = Grid.parseCharacterGrid(Input.asString("day8.txt")).map(_ - '0')

  println(solvePart1(sample)) // 21
  println(solvePart1(input)) // 1669

  println(solvePart2(sample)) // 8
  println(solvePart2(input)) // 331344

}
