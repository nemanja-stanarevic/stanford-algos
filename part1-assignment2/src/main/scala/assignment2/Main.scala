package assignment2

import scala.annotation.tailrec

/** In-place quick sort using mutable array. */

object Main extends App {
  type Result = (Array[Int], Long)

  val firstElementPivot = (a: Array[Int], l: Int, r: Int) ⇒ l
  val lastElementPivot = (a: Array[Int], l: Int, r: Int) ⇒ r
  val medianPivot = (a: Array[Int], l: Int, r: Int) ⇒ {
    val first = a(l)
    val last = a(r)
    val middleIndex = l + Math.ceil((r - l + 1) / 2.0).toInt - 1
    val middle = a(middleIndex)
    val max = Math.max(Math.max(first, last), middle)
    val min = Math.min(Math.min(first, last), middle)
    if (first > min && first < max)
      l
    else if (middle > min && middle < max)
      middleIndex
    else
      r
  }

  def quickSort(a: Array[Int], pivot: (Array[Int], Int, Int) ⇒ Int): Result = {
    def swap(l: Int, r: Int): Unit = {
      val temp = a(r)
      a(r) = a(l)
      a(l) = temp
    }
    def partition(l: Int, r: Int): Int = {
      val pivot = a(l)
      var i = l + 1
      for (j ← l + 1 to r)
        if (a(j) < pivot) {
          swap(i, j)
          i += 1
        }
      swap(l, i - 1)
      i - 1
    }
    def quick(l: Int, r: Int): Long = {
      if (r - l <= 0)
        0L
      else {
        val pivotIndex = pivot(a, l, r)
        swap(l, pivotIndex)
        val p = partition(l, r)
        (r - l) + quick(l, p - 1) + quick(p + 1, r)
      }
    }
    (a, quick(0, a.length - 1))
  }

  @tailrec def correctlySorted(a: Array[Int]): Boolean =
    (a.length == 1) || ((a.head < a.tail.head) && correctlySorted(a.tail))

  val fileName = if (args.isEmpty) "data.txt" else args.head
  val input = scala.io.Source.fromFile(fileName)
    .getLines
    .foldLeft(Array[Int]()) {
      (acc, elem) ⇒ acc :+ elem.toInt
    }

  val pivots = List(firstElementPivot, lastElementPivot, medianPivot)

  for (pivot ← pivots)
    quickSort(input.clone, pivot) match {
      case (array, comps) ⇒
        println(s"Correctly sorted? ${correctlySorted(array)}")
        println(s"# of comparisons? ${comps}")
    }
}