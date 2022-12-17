package aoc.util

sealed abstract class Direction2D(val delta: Delta2D) extends Product with Serializable

object Direction2D {

  sealed abstract class Vertical(delta: Delta2D) extends Direction2D(delta)
  sealed abstract class Horizontal(delta: Delta2D) extends Direction2D(delta)

  sealed abstract class Diagonal(delta: Delta2D) extends Direction2D(delta)

  case object Up extends Vertical(Delta2D(dx = 0, dy = 1))
  case object Right extends Horizontal(Delta2D(dx = 1, dy = 0))
  case object Down extends Vertical(Delta2D(dx = 0, dy = -1))
  case object Left extends Horizontal(Delta2D(dx = -1, dy = 0))

  case object UpRight extends Diagonal(Delta2D(dx = 1, dy = 1))
  case object DownRight extends Diagonal(Delta2D(dx = 1, dy = -1))
  case object DownLeft extends Diagonal(Delta2D(dx = -1, dy = -1))
  case object UpLeft extends Diagonal(Delta2D(dx = -1, dy = 1))

}
