package assignment1

import scala.annotation.tailrec

object Main extends App {
  type result = (Vector[Int], Long)

  def sortAndCountInversions(xs: Vector[Int]): result = {
    if (xs.isEmpty || xs.length == 1)
      (xs, 0)
    else {
      val leftHalf = xs.take(xs.length / 2)
      val rightHalf = xs.takeRight(xs.length - xs.length / 2)
      mergeAndCountInversions(
        sortAndCountInversions(leftHalf),
        sortAndCountInversions(rightHalf))
    }
  }

  def mergeAndCountInversions(xs: result, ys: result): result = {
    @tailrec def mergeRec(xs: result, ys: result, acc: result): result = {
      if (xs._1.isEmpty)
        (acc._1 ++ ys._1, xs._2 + ys._2 + acc._2)
      else if (ys._1.isEmpty)
        (acc._1 ++ xs._1, xs._2 + ys._2 + acc._2)
      else if (xs._1.head <= ys._1.head)
        mergeRec((xs._1.tail, xs._2),
          ys,
          (acc._1 :+ xs._1.head, acc._2))
      else
        mergeRec(xs,
          (ys._1.tail, ys._2),
          (acc._1 :+ ys._1.head, acc._2 + xs._1.length))
    }
    mergeRec(xs, ys, (Vector[Int](), 0))
  }

  val fileName =
    if (args.isEmpty) "data.txt"
    else args.head

  val input = scala.io.Source.fromFile(fileName)
    .getLines
    .foldLeft(Vector[Int]()) {
      (acc, elem) ⇒ acc :+ elem.toInt
    }

  val res = sortAndCountInversions(input)
  println(s"Correctly sorted? ${res._1 == input.sorted}")
  println(s"# of inversions? ${res._2}")
}