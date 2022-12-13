package aoc.util

import cats.syntax.all._
import io.circe.Json
import io.circe.parser.{parse => parseJSON}

sealed trait TreeNode[+A] {
  def map[B](f: A => B): TreeNode[B]
}

object TreeNode {

  case class Leaf[A](value: A) extends TreeNode[A] {
    def map[B](f: A => B): TreeNode[B] = Leaf(f(value))
  }

  case class InternalNode[A](children: List[TreeNode[A]]) extends TreeNode[A] {
    def map[B](f: A => B): TreeNode[B] = InternalNode(children.map(_.map(f)))
  }

  def parse(raw: String): Either[String, TreeNode[String]] = {
    def toTree(json: Json): Either[String, TreeNode[String]] =
      json.fold(
        jsonNull = Left("Unexpected token: null"),
        jsonBoolean = v => Right(Leaf(v.toString)),
        jsonNumber = v => Right(Leaf(v.toString)),
        jsonString = v => Right(Leaf(v)),
        jsonArray = _.traverse(toTree).map(_.toList).map(InternalNode(_)),
        jsonObject = o => Left(s"Unexpected token: object: $o")
      )

    parseJSON(raw)
      .leftMap(e => s"Not a valid tree: ${e.message}")
      .flatMap(toTree)
  }
}

