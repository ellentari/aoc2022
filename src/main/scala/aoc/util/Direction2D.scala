package aoc.util

sealed abstract class Direction2D(val delta: Delta2D) extends Product with Serializable

object Direction2D {
  case object Up extends Direction2D(Delta2D(dx = 0, dy = -1))
  case object UpRight extends Direction2D(Delta2D(dx = 1, dy = -1))
  case object Right extends Direction2D(Delta2D(dx = 1, dy = 0))
  case object DownRight extends Direction2D(Delta2D(dx = 1, dy = 1))
  case object Down extends Direction2D(Delta2D(dx = 0, dy = 1))
  case object DownLeft extends Direction2D(Delta2D(dx = -1, dy = 1))
  case object Left extends Direction2D(Delta2D(dx = -1, dy = 0))
}
