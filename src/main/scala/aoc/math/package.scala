package aoc

import scala.annotation.tailrec

package object math {

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

}
