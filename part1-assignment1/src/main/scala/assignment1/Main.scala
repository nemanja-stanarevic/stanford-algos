package assignment1

import scala.annotation.tailrec

object Main extends App {
  type result = (Vector[Int], Long)

  def sortAndCountInversions(xs: Vector[Int]): result = {
    if (xs.isEmpty || xs.length == 1)
      (xs, 0)
    else {
      val leftHalf = xs.take(xs.length / 2)
      val rightHalf = xs.drop(xs.length / 2)
      mergeAndCountInversions(
        sortAndCountInversions(leftHalf),
        sortAndCountInversions(rightHalf))
    }
  }

  def mergeAndCountInversions(xs: result, ys: result): result = {
    @tailrec def mergeRec(xs: result, ys: result, acc: result): result =
      (xs, ys, acc) match {
        case ((Vector(), xsCount), (ysVector, ysCount), (accVector, accCount)) ⇒
          (accVector ++ ysVector, xsCount + ysCount + accCount)
        case ((xsVector, xsCount), (Vector(), ysCount), (accVector, accCount)) ⇒
          (accVector ++ xsVector, xsCount + ysCount + accCount)
        case ((x +: xtail, xsCount), (y +: ytail, ysCount), (accVector, accCount)) 
          if (x <= y) ⇒
          mergeRec((xtail, xsCount),
            ys,
            (accVector :+ x, accCount))
        case ((x +: xtail, xsCount), (y +: ytail, ysCount), (accVector, accCount)) ⇒
          mergeRec(xs,
            (ytail, ysCount),
            (accVector :+ y, accCount + xtail.length + 1))
      }
    mergeRec(xs, ys, (Vector[Int](), 0))
  }

  val fileName = if (args.isEmpty) "data.txt" else args.head
  val input = scala.io.Source.fromFile(fileName)
    .getLines
    .foldLeft(Vector[Int]()) {
      (acc, elem) ⇒ acc :+ elem.toInt
    }

  val res = sortAndCountInversions(input)
  println(s"Correctly sorted? ${res._1 == input.sorted}")
  println(s"# of inversions? ${res._2}")
}