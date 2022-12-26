package aoc.util

case class GridMap[A](underlying: Map[Coordinate2D, A]) {

  def minX: Int = minXOption.getOrElse(0)
  def minXOption: Option[Int] = underlying.keySet.map(_.x).minOption

  def maxX: Int = underlying.keySet.map(_.x).maxOption.getOrElse(0)
  def maxXOption: Option[Int] = underlying.keySet.map(_.x).maxOption

  def minY: Int = minYOption.getOrElse(0)
  def minYOption: Option[Int] = underlying.keySet.map(_.y).minOption

  def maxY: Int = maxYOption.getOrElse(0)
  def maxYOption: Option[Int] = underlying.keySet.map(_.y).maxOption

  def height: Int = maxY - minY + 1
  def width: Int = maxX - minX + 1

  def contains(cc: Coordinate2D): Boolean = underlying.contains(cc)

  def rowAtY(y: Int): List[Coordinate2D] =
    underlying.keys.view.filter(_.y == y).toList.sortBy(_.x)

  def apply(coordinate: Coordinate2D): A = underlying(coordinate)
  def get(coordinate: Coordinate2D): Option[A] = underlying.get(coordinate)

  def ++(other: Iterable[(Coordinate2D, A)]): GridMap[A] = GridMap(underlying ++ other)
  def ++(other: GridMap[A]): GridMap[A] = GridMap(underlying ++ other.underlying)

  def --(keys: Iterable[Coordinate2D]): GridMap[A] = GridMap(underlying -- keys)

  def ys: Range = if (underlying.isEmpty) (0 until 0) else maxY to minY by -1
  def xs: Range = if (underlying.isEmpty) (0 until 0) else minX to maxX

  def map[B](f: A => B): GridMap[B] = GridMap(underlying.view.mapValues(f).toMap)
  def mapKeys(f: Coordinate2D => Coordinate2D): GridMap[A] = GridMap(underlying.view.map(t => f(t._1) -> t._2).toMap)
  def map2[B](f: ((Coordinate2D, A)) => (Coordinate2D, B)): GridMap[B] = GridMap(underlying.map(f))
  def mapCoordinates(f: Coordinate2D => Coordinate2D): GridMap[A] =
    GridMap(underlying.toList.map(kv => f(kv._1) -> kv._2).toMap)

  def updated(coordinate: Coordinate2D, a: A): GridMap[A] =
    GridMap(underlying.updated(coordinate, a))

  def updated(coordinate: Coordinate2D, update: A => A): GridMap[A] =
    GridMap(underlying.updatedWith(coordinate)(_.map(update)))

  def adjacent4(coordinate: Coordinate2D): List[Coordinate2D] =
    coordinate.adjacent4.filter(underlying.contains)

  def adjacent8(coordinate: Coordinate2D): List[Coordinate2D] =
    coordinate.adjacent8.filter(underlying.contains)

  def find(predicate: A => Boolean): Option[Coordinate2D] =
    underlying.find { case (_, v) => predicate(v) }.map(_._1)

  def filter(p: ((Coordinate2D, A)) => Boolean): GridMap[A] =
    GridMap(underlying.filter(p))

  def findAll(predicate: A => Boolean): List[Coordinate2D] =
    underlying.filter { case (_, v) => predicate(v) }.keys.toList

  def mkString(emptyChar: Char): String =
    if (underlying.isEmpty) ""
    else
      ys
        .map(y => xs
          .map(x => get(Coordinate2D(x, y)).getOrElse(emptyChar))
          .mkString("")
        ).mkString("\n")
}

object GridMap {

  def empty[A]: GridMap[A] = GridMap[A](Map.empty)

  def fromGrid[A](grid: Grid[A]): GridMap[A] =
    GridMap(grid.rowColumnIndices
      .map(i => Coordinate2D(i.column, -i.row) -> grid(i))
      .toMap)

  def parseCharacterGrid(s: String): GridMap[Char] =
    fromGrid(Grid.parseCharacterGrid(s))

}


