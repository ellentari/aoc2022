package aoc.util

import scala.annotation.tailrec

sealed abstract class Direction2D(val delta: Delta2D) extends Product with Serializable

object Direction2D {

  sealed abstract class HorizontalOrVertical(delta: Delta2D) extends Direction2D(delta) {

    def rotateClockwise: HorizontalOrVertical
    def rotateCounterclockwise: HorizontalOrVertical
    def opposite: HorizontalOrVertical

    def rotateClockwise(times: Int): HorizontalOrVertical = {
      @tailrec
      def loop(dir: HorizontalOrVertical, times: Int): HorizontalOrVertical =
        if (times == 0) dir
        else loop(dir.rotateClockwise, times - 1)

      loop(this, times % 4)
    }

    def rotateCounterclockwise(times: Int): HorizontalOrVertical = {
      @tailrec
      def loop(dir: HorizontalOrVertical, times: Int): HorizontalOrVertical =
        if (times == 0) dir
        else loop(dir.rotateCounterclockwise, times - 1)

      loop(this, times % 4)
    }



    def turnRight: HorizontalOrVertical = rotateClockwise
    def turnRight(times: Int): HorizontalOrVertical = rotateClockwise(times)
    def turnLeft: HorizontalOrVertical = rotateCounterclockwise
    def turnLeft(times: Int): HorizontalOrVertical = rotateCounterclockwise(times)

  }

  sealed abstract class Vertical(delta: Delta2D) extends HorizontalOrVertical(delta)
  sealed abstract class Horizontal(delta: Delta2D) extends HorizontalOrVertical(delta)

  sealed abstract class Diagonal(delta: Delta2D) extends Direction2D(delta)

  case object Up extends Vertical(Delta2D(dx = 0, dy = 1)) {
    def rotateClockwise: HorizontalOrVertical = Right
    def rotateCounterclockwise: HorizontalOrVertical = Left
    def opposite: HorizontalOrVertical = Down
  }

  case object Right extends Horizontal(Delta2D(dx = 1, dy = 0)) {
    def rotateClockwise: HorizontalOrVertical = Down
    def rotateCounterclockwise: HorizontalOrVertical = Up
    def opposite: HorizontalOrVertical = Left
  }

  case object Down extends Vertical(Delta2D(dx = 0, dy = -1)) {
    def rotateClockwise: HorizontalOrVertical = Left
    def rotateCounterclockwise: HorizontalOrVertical = Right
    def opposite: HorizontalOrVertical = Up
  }

  case object Left extends Horizontal(Delta2D(dx = -1, dy = 0)) {
    def rotateClockwise: HorizontalOrVertical = Up
    def rotateCounterclockwise: HorizontalOrVertical = Down
    def opposite: HorizontalOrVertical = Right
  }

  case object UpRight extends Diagonal(Delta2D(dx = 1, dy = 1))
  case object DownRight extends Diagonal(Delta2D(dx = 1, dy = -1))
  case object DownLeft extends Diagonal(Delta2D(dx = -1, dy = -1))
  case object UpLeft extends Diagonal(Delta2D(dx = -1, dy = 1))

}
