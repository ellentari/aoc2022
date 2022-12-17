package aoc.util

case class GridMap[A](map: Map[Coordinate2D, A]) {

  def minX: Int = minXOption.getOrElse(0)
  def minXOption: Option[Int] = map.keySet.map(_.x).minOption

  def maxX: Int = map.keySet.map(_.x).maxOption.getOrElse(0)
  def maxXOption: Option[Int] = map.keySet.map(_.x).maxOption

  def minY: Int = minYOption.getOrElse(0)
  def minYOption: Option[Int] = map.keySet.map(_.y).minOption

  def maxY: Int = maxYOption.getOrElse(0)
  def maxYOption: Option[Int] = map.keySet.map(_.y).maxOption

  def height: Int = maxY - minY + 1
  def width: Int = maxX - minX + 1

  def apply(coordinate: Coordinate2D): A = map(coordinate)
  def get(coordinate: Coordinate2D): Option[A] = map.get(coordinate)

  def ++(other: Iterable[(Coordinate2D, A)]): GridMap[A] = GridMap(map ++ other)
  def ++(other: GridMap[A]): GridMap[A] = GridMap(map ++ other.map)

  def map[B](f: A => B): GridMap[B] = GridMap(map.view.mapValues(f).toMap)
  def mapCoordinates(f: Coordinate2D => Coordinate2D): GridMap[A] =
    GridMap(map.toList.map(kv => f(kv._1) -> kv._2).toMap)

  def updated(coordinate: Coordinate2D, a: A): GridMap[A] =
    GridMap(map.updated(coordinate, a))

  def updated(coordinate: Coordinate2D, update: A => A): GridMap[A] =
    GridMap(map.updatedWith(coordinate)(_.map(update)))

  def adjacent4(coordinate: Coordinate2D): List[Coordinate2D] =
    coordinate.adjacent4.filter(map.contains)

  def adjacent8(coordinate: Coordinate2D): List[Coordinate2D] =
    coordinate.adjacent8.filter(map.contains)

  def find(predicate: A => Boolean): Option[Coordinate2D] =
    map.find { case (_, v) => predicate(v) }.map(_._1)

  def findAll(predicate: A => Boolean): List[Coordinate2D] =
    map.filter { case (_, v) => predicate(v) }.keys.toList

  override def toString: String =
    if (map.isEmpty) ""
    else
      (maxY to minY by -1)
        .map(y => (minX to maxX)
          .map(x => get(Coordinate2D(x, y)).getOrElse('.'))
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


