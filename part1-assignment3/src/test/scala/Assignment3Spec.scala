import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import assignment3.Main._

class Assignment3Spec extends FlatSpec with Matchers {
  def runTest(setup: String) = {
    val graph = setup
      .split('\n')
      .foldLeft(Graph[Set[Int]](Vector(), Vector())) {
        (graph, inputLine) ⇒
          val list = inputLine.split(Array(' ', '\t', ',')).foldLeft(Vector[Int]()) {
            (v, vertexStr) ⇒ v :+ vertexStr.toInt
          }
          // -1s to adjust for zero-based vector indexing 
          val vertex = list.head
          val adjecency = list.tail
          val edges = adjecency map { destVertex ⇒ (vertex - 1, destVertex - 1) }
          Graph(
            graph.vertices :+ Set(vertex),
            graph.edges ++ edges)
      }

    // number of iterations should be n^2 ln(n) where n is number of vertices
    val n = graph.vertices.size
    val iterations = ( Math.pow(n, 2) * Math.log(n) ).toInt

    var minCuts = Int.MaxValue
    import scala.util.Random
    for (iter <- 0 until iterations) {
      implicit val rng = new Random()
      val result = ranomizedContraction(graph)
      val cuts = countCuts(result)
      if (cuts < minCuts) {
        minCuts = cuts
      }
    }
    minCuts
  }

  "Ranomized contraction" should "work for 'two x-ed boxes' connected with two edges" in {
    runTest("""1 2 6 5
              |2 1 5 6 3
              |3 2 7 8 4
              |4 3 7 8
              |5 1 2 6
              |6 5 1 2 7
              |7 6 3 4 8
              |8 7 3 4""".stripMargin('|')) shouldEqual 2
  }

  "Ranomized contraction" should "work for 'three x-ed boxes' graph" in {
    runTest("""1 2 6 5
              |2 1 5 6 3 7
              |3 2 7 8 4 6
              |4 3 7 8
              |5 1 2 6
              |6 5 1 2 7 3
              |7 6 3 4 8 2
              |8 7 3 4""".stripMargin('|')) shouldEqual 3
  }

  "Ranomized contraction" should "work for tree graph" in {
    runTest("""1 2 3
              |2 1 4 5
              |3 1 6 7
              |4 2
              |5 2
              |6 3
              |7 3""".stripMargin('|')) shouldEqual 1
  }

  "Ranomized contraction" should "work for pyramid graph" in {
    runTest("""1 2 3
              |2 1 4 5 3
              |3 1 6 7 2
              |4 2 5
              |5 2 6 4
              |6 3 5 7
              |7 3 6""".stripMargin('|')) shouldEqual 2
  }
}
