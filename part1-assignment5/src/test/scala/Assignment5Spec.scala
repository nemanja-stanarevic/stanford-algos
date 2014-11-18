import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import assignment5.Main._

class Assignment5Spec extends FlatSpec with Matchers {

  /*def runTest(graphStr: String) : Vector[Int] = {
    val graph = parseGraph(graphStr.stripMargin('|').lines)
    val leader = kosaraju(graph)
    // return sizes of top SCCs
    leader.values
      .map { vect ⇒ vect.size }
      .toVector
      .sorted(scala.math.Ordering[Label].reverse)
  }
  
  "Kosaraju" should "for a small acyclic graph" in {
    runTest("""1 7
              |2 5
              |3 9
              |4 1
              |5 8
              |6 3
              |6 8
              |7 10
              |7 9
              |8 2
              |9 6
              |10 4""") shouldEqual Vector(4, 3, 3)
  }

  "Kosaraju" should "work for a small cyclic bi-directional graph" in {
    runTest("""1 2
              |2 3
              |3 4
              |4 5
              |5 6
              |6 7
              |7 1
              |1 7
              |7 6
              |6 5
              |5 4
              |4 3
              |3 2
              |2 1""") shouldEqual Vector(7)
  }*/
}
