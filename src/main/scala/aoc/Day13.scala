package aoc

import aoc.util.TreeNode
import aoc.util.TreeNode._
import cats.Order
import cats.instances.order._
import cats.syntax.all._

import scala.annotation.tailrec

object Day13 extends App {

  type Packet = TreeNode[Int]

  implicit val orderPacket: Order[Packet] = Order.from(comparePackets)

  def solvePart1(input: List[(Packet, Packet)]): Int =
    input
      .zipWithIndex
      .filter { case ((p1, p2), _) => p1 <= p2 }
      .foldMap(_._2 + 1)

  def solvePart2(input: List[Packet]): Int = {
    val divider1 = InternalNode(List(InternalNode(List(Leaf(2)))))
    val divider2 = InternalNode(List(InternalNode(List(Leaf(6)))))

    (divider1 :: divider2 :: input)
      .zipWithIndex
      .sortBy(_._1)
      .zipWithIndex
      .filter(t => t._1._2 == 0 || t._1._2 == 1)
      .map(_._2 + 1)
      .product
  }

  private def comparePackets(packet1: Packet, packet2: Packet): Int = {

    @tailrec
    def compareChildren(children1: List[TreeNode[Int]], children2: List[TreeNode[Int]]): Int =
      (children1, children2) match {
        case (Nil, Nil) => 0
        case (_, Nil) => 1
        case (Nil, _) => -1
        case (h1 :: t1, h2 :: t2) =>
          val result = comparePackets(h1, h2)

          if (result != 0) result
          else compareChildren(t1, t2)
      }

    (packet1, packet2) match {
      case (Leaf(v1), Leaf(v2)) =>
        Order[Int].compare(v1, v2)
      case (Leaf(v), _: InternalNode[_]) =>
        comparePackets(InternalNode(List(Leaf(v))), packet2)
      case (_: InternalNode[_], Leaf(v)) =>
        comparePackets(packet1, InternalNode(List(Leaf(v))))
      case (node1: InternalNode[Int], node2: InternalNode[Int]) =>
        compareChildren(node1.children, node2.children)
    }
  }

  private def parsePackets(raw: String): List[(Packet, Packet)] = {
    def parsePacket(raw: String): Packet =
      TreeNode.parse(raw)
        .fold(e => throw new RuntimeException(s"Failed to parse packet: $e"), _.map(_.toInt))

    raw
      .split("\n\n")
      .map { s =>
        val parts = s.split("\n")

        (parsePacket(parts(0)), parsePacket(parts(1)))
      }
      .toList
  }

  private val sample = parsePackets(Input.asString("day13_sample.txt"))
  private val input = parsePackets(Input.asString("day13.txt"))

  println(solvePart1(sample)) // 13
  println(solvePart1(input)) // 6070
  println(solvePart2(sample.flatMap(t => List(t._1, t._2)))) // 140
  println(solvePart2(input.flatMap(t => List(t._1, t._2)))) // 20758

}
