package aoc.util

case class Grid[A](rows: IndexedSeq[IndexedSeq[A]]) {
  
  def height: Int = rows.length
  def width: Int = rows.headOption.fold(0)(_.length)

  def apply(index: Index): A = apply(index.row, index.column)
  def apply(row: Int, col: Int): A = rows(row)(col)

  def columns: IndexedSeq[IndexedSeq[A]] = (0 until width).map(col => rows.map(_(col)))

  def map[B](f: A => B): Grid[B] = Grid(rows.map(_.map(f)))

  def updated(index: Index, a: A): Grid[A] = updated(index.row, index.column, a)
  def updated(row: Int, col: Int, a: A): Grid[A] = updated(row, col, _ => a)

  def updated(index: Index, f: A => A): Grid[A] = updated(index.row, index.column, f)
  def updated(row: Int, col: Int, f: A => A): Grid[A] =
    Grid(rows.updated(row, rows(row).updated(col, f(apply(row, col)))))

  def rowIndices: Range = rows.indices
  def rowIndicesAbove(row: Int): Range = row - 1 to 0 by -1
  def rowIndicesBelow(row: Int): Range = row + 1 until height

  def columnIndices: Range = 0 until width
  def columnIndicesLeft(col: Int): Range = col - 1 to 0 by -1
  def columnIndicesRight(col: Int): Range = col + 1 until width

  def rowColumnIndices: List[Index] = rowIndices.flatMap(row => rows(row).indices.map(Index(row, _))).toList

  def indicesAbove(index: Index): IndexedSeq[Index] = indicesAbove(index.row, index.column)
  def indicesAbove(row: Int, col: Int): IndexedSeq[Index] = rowIndicesAbove(row).map(Index(_, col))

  def indicesBelow(index: Index): IndexedSeq[Index] = indicesBelow(index.row, index.column)
  def indicesBelow(row: Int, col: Int): IndexedSeq[Index] = rowIndicesBelow(row).map(Index(_, col))

  def indicesLeft(index: Index): IndexedSeq[Index] = indicesLeft(index.row, index.column)
  def indicesLeft(row: Int, col: Int): IndexedSeq[Index] = columnIndicesLeft(col).map(Index(row, _))

  def indicesRight(index: Index): IndexedSeq[Index] = indicesRight(index.row, index.column)
  def indicesRight(row: Int, col: Int): IndexedSeq[Index] = columnIndicesRight(col).map(Index(row, _))

  def indicesDiagonalUpRight(index: Index): IndexedSeq[Index] =
    indicesDiagonalUpRight(index.row, index.column)
  def indicesDiagonalUpRight(row: Int, col: Int): IndexedSeq[Index] =
    rowIndicesAbove(row).zip(columnIndicesRight(col)).map((Index.apply _).tupled)

  def indicesDiagonalDownRight(index: Index): IndexedSeq[Index] =
    indicesDiagonalDownRight(index.row, index.column)
  def indicesDiagonalDownRight(row: Int, col: Int): IndexedSeq[Index] =
    rowIndicesBelow(row).zip(columnIndicesRight(col)).map((Index.apply _).tupled)

  def indicesDiagonalDownLeft(index: Index): IndexedSeq[Index] =
    indicesDiagonalDownLeft(index.row, index.column)
  def indicesDiagonalDownLeft(row: Int, col: Int): IndexedSeq[Index] =
    rowIndicesBelow(row).zip(columnIndicesLeft(col)).map((Index.apply _).tupled)

  def indicesDiagonalUpLeft(index: Index): IndexedSeq[Index] = indicesDiagonalUpLeft(index.row, index.column)
  def indicesDiagonalUpLeft(row: Int, col: Int): IndexedSeq[Index] =
    rowIndicesAbove(row).zip(columnIndicesLeft(col)).map((Index.apply _).tupled)

  def adjacent4(index: Index): List[Index] = adjacent4(index.row, index.column)
  def adjacent4(row: Int, col: Int): List[Index] =
    List((row - 1, col), (row, col + 1), (row + 1, col), (row, col - 1))
      .filter(isWithinGrid)
      .map((Index.apply _).tupled)

  def adjacent8(index: Index): List[Index] = adjacent8(index.row, index.column)
  def adjacent8(row: Int, col: Int): List[Index] =
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
      .map((Index.apply _).tupled)

  def indexOf(predicate: A => Boolean): Option[Index] =
    rowColumnIndices.find(index => predicate(apply(index)))

  def findShortestPath(
    start: Index,
    shouldVisit: (A, A) => Boolean,
    isEnd: Index => Boolean): Option[Int] = {
    @tailrec
    def bfs(queue: Queue[(Index, Int)], seen: Set[Index]): Option[Int] =
      queue.dequeueOption match {
        case None => None
        case Some(((index, length), tail)) =>
          if (isEnd(index)) Some(length)
          else {
            val toVisit = adjacent4(index)
              .filterNot(seen.contains)
              .filter(adj => shouldVisit(apply(index), apply(adj)))

            bfs(tail ++ toVisit.map(_ -> (length + 1)), seen ++ toVisit)
          }
      }

    bfs(Queue((start, 0)), Set(start))
  }

  private def isWithinGrid(cc: (Int, Int)) =
    cc._1 >= 0 && cc._1 < rows.length && cc._2 >= 0 && cc._2 < rows(cc._1).length

}

object Grid {

  case class Index(row: Int, column: Int)

  def parseCharacterGrid(raw: String): Grid[Char] =
    Grid(raw.split("\n").map(_.toVector).toVector)

}
