import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import assignment1.Main._

class Assignment1Spec extends FlatSpec with Matchers {
  "Sort and Count Inversions" should "work for empty set" in {
    sortAndCountInversions(Vector()) shouldEqual (Vector[Int](), 0)
  }
  "Sort and Count Inversions" should "work for an one-element set" in {
    sortAndCountInversions(Vector(1)) shouldEqual (Vector[Int](1), 0)
  }
  "Sort and Count Inversions" should "work for a two element set without inversion" in {
    sortAndCountInversions(Vector(1, 2)) shouldEqual (Vector[Int](1, 2), 0)
  }
  "Sort and Count Inversions" should "work for a two element set with inversion" in {
    sortAndCountInversions(Vector(2, 1)) shouldEqual (Vector[Int](1, 2), 1)
  }
  "Sort and Count Inversions" should "work for a {1, 3, 2} set" in {
    sortAndCountInversions(Vector(1, 3, 2)) shouldEqual (Vector[Int](1, 2, 3), 1)
  }
  "Sort and Count Inversions" should "work for a {3, 1, 2} set" in {
    sortAndCountInversions(Vector(3, 1, 2)) shouldEqual (Vector[Int](1, 2, 3), 2)
  }
  "Sort and Count Inversions" should "work for a {3, 2, 1} set" in {
    sortAndCountInversions(Vector(3, 2, 1)) shouldEqual (Vector[Int](1, 2, 3), 3)
  }

  val bigSet = scala.io.Source.fromFile("data.txt").getLines.foldLeft(Vector[Int]()){ (acc, elem) ⇒
    acc :+ elem.toInt
  }

  "Sort and Count Inversions" should "work for a big set" in {
    sortAndCountInversions(bigSet) shouldEqual (bigSet.sorted, 2407905288L)
  }
}
