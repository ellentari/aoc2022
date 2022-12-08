package aoc.util

case class Grid[A](rows: IndexedSeq[IndexedSeq[A]]) {
  
  def height: Int = rows.length
  def width: Int = rows.headOption.fold(0)(_.length)

  def apply(cc: (Int, Int)): A = apply(cc._1, cc._2)
  def apply(row: Int, col: Int): A = rows(row)(col)

  def columns: IndexedSeq[IndexedSeq[A]] = (0 until width).map(col => rows.map(_(col)))

  def map[B](f: A => B): Grid[B] = Grid(rows.map(_.map(f)))

  def updated(cc: (Int, Int), a: A): Grid[A] = updated(cc._1, cc._2, a)
  def updated(row: Int, col: Int, a: A): Grid[A] = updated(row, col, _ => a)

  def updated(cc: (Int, Int), f: A => A): Grid[A] = updated(cc._1, cc._2, f)
  def updated(row: Int, col: Int, f: A => A): Grid[A] =
    Grid(rows.updated(row, rows(row).updated(col, f(apply(row, col)))))

  def rowIndices: Range = rows.indices
  def rowIndicesAbove(row: Int): Range = row - 1 to 0 by -1
  def rowIndicesBelow(row: Int): Range = row + 1 until height

  def columnIndices: Range = 0 until width
  def columnIndicesLeft(col: Int): Range = col - 1 to 0 by -1
  def columnIndicesRight(col: Int): Range = col + 1 until width

  def rowColumnIndices: List[(Int, Int)] = rowIndices.flatMap(row => rows(row).indices.map(row -> _)).toList

  def indicesAbove(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesAbove(cc._1, cc._2)
  def indicesAbove(row: Int, col: Int): IndexedSeq[(Int, Int)] = rowIndicesAbove(row).map(_ -> col)

  def indicesBelow(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesBelow(cc._1, cc._2)
  def indicesBelow(row: Int, col: Int): IndexedSeq[(Int, Int)] = rowIndicesBelow(row).map(_ -> col)

  def indicesLeft(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesLeft(cc._1, cc._2)
  def indicesLeft(row: Int, col: Int): IndexedSeq[(Int, Int)] = columnIndicesLeft(col).map(row -> _)

  def indicesRight(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesRight(cc._1, cc._2)
  def indicesRight(row: Int, col: Int): IndexedSeq[(Int, Int)] = columnIndicesRight(col).map(row -> _)

  def indicesDiagonalUpRight(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesDiagonalUpRight(cc._1, cc._2)
  def indicesDiagonalUpRight(row: Int, col: Int): IndexedSeq[(Int, Int)] =
    rowIndicesAbove(row).zip(columnIndicesRight(col))

  def indicesDiagonalDownRight(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesDiagonalDownRight(cc._1, cc._2)
  def indicesDiagonalDownRight(row: Int, col: Int): IndexedSeq[(Int, Int)] =
    rowIndicesBelow(row).zip(columnIndicesRight(col))

  def indicesDiagonalDownLeft(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesDiagonalDownLeft(cc._1, cc._2)
  def indicesDiagonalDownLeft(row: Int, col: Int): IndexedSeq[(Int, Int)] =
    rowIndicesBelow(row).zip(columnIndicesLeft(col))

  def indicesDiagonalUpLeft(cc: (Int, Int)): IndexedSeq[(Int, Int)] = indicesDiagonalUpLeft(cc._1, cc._2)
  def indicesDiagonalUpLeft(row: Int, col: Int): IndexedSeq[(Int, Int)] =
    rowIndicesAbove(row).zip(columnIndicesLeft(col))

  def adjacent4(cc: (Int, Int)): List[(Int, Int)] = adjacent4(cc._1, cc._2)
  def adjacent4(row: Int, col: Int): List[(Int, Int)] =
    List((row - 1, col), (row, col + 1), (row + 1, col), (row, col - 1)).filter(isWithinGrid)

  def adjacent8(cc: (Int, Int)): List[(Int, Int)] = adjacent8(cc._1, cc._2)
  def adjacent8(row: Int, col: Int): List[(Int, Int)] =
    List(
      (row - 1, col),
      (row - 1, col + 1),
      (row, col + 1),
      (row + 1, col + 1),
      (row + 1, col),
      (row + 1, col - 1),
      (row, col - 1),
      (row - 1, col - 1)
    )
      .filter(isWithinGrid)

  private def isWithinGrid(cc: (Int, Int)) =
    cc._1 >= 0 && cc._1 < rows.length && cc._2 >= 0 && cc._2 < rows(cc._1).length

}

object Grid {

  def parseCharacterGrid(raw: String): Grid[Char] =
    Grid(raw.split("\n").map(_.toVector).toVector)

}
