package assignment6

import scala.collection.immutable._
import scala.annotation.tailrec
import scala.util.control.Breaks._

object Main1 extends App {
  def loadData(fileName: String): Vector[Long] =
    parseData(scala.io.Source.fromFile(fileName).getLines)

  def parseData(lines: Iterator[String]): Vector[Long] =
    lines.foldLeft(Vector[Long]()) { (vector, line) => vector :+ line.toLong }

  def buildHashSet(input: Vector[Long]): HashSet[Long] =
    input.foldLeft(HashSet[Long]()) { (set, number) => set + number }

  val fileName = if (args.isEmpty) "data-1.txt" else args.head
  var input = loadData(fileName)

  val hashSet = buildHashSet(input)
  var count = 0
  for (target <- -10000 to 10000) {
    breakable {
      for (element <- input) {
        if ( (hashSet contains (target - element)) && ((target - element) != element) ) {
          count = count + 1
          //println(s" count = $count, target = $target, elements = $element and ${target-element} ")
          break
        }
      }
    }
  }
  println(s"Problem 1) count = $count")
}

