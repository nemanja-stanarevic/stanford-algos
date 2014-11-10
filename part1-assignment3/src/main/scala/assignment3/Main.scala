package assignment3

import scala.collection.immutable._
import scala.util.Random
import scala.annotation.tailrec

object Main extends App {
  case class Graph[T](
    vertices: Vector[T],
    edges: Vector[(Int, Int)])

  def loadGraph(fileName: String): Graph[Set[Int]] =
    scala.io.Source.fromFile(fileName)
      .getLines
      .foldLeft(Graph[Set[Int]](Vector(), Vector())) {
        (graph, inputLine) ⇒
          val list = inputLine.split(Array(' ', '\t', ',')).foldLeft(Vector[Int]()) {
            (v, vertexStr) ⇒ v :+ vertexStr.toInt
          }
          val vertex = list.head
          val adjecency = list.tail
          // -1s to adjust for zero-based vector indexing 
          val edges = adjecency map { destVertex ⇒ (vertex - 1, destVertex - 1) }
          Graph(
            graph.vertices :+ Set(vertex),
            graph.edges ++ edges)
      }

  @tailrec def ranomizedContraction(graph: Graph[Set[Int]])
    (implicit rng: Random): Graph[Set[Int]] =
    if (graph.vertices.size <= 2)
      graph
    else {
      // pick an edge at random
      val edgeIndex = rng.nextInt(graph.edges.size)
      val (v1, v2) = graph.edges(edgeIndex)
      val newVertices = graph.vertices
        // contract v1 and v2 into a single vertex
        .updated(v1, graph.vertices(v1) ++ graph.vertices(v2))
        // remove v2
        .patch(from = v2, patch = Nil, replaced = 1)
      val newEdges = graph.edges
        // Contract the edges that connect the two vertices
        .map {
          case (x, y) if (x == v2) ⇒ (v1, y)
          case (x, y) if (y == v2) ⇒ (x, v1)
          case edge                ⇒ edge
        }
        // Update edges with index greater than v2
        .map {
          case (x, y) if (x > v2) & (y > v2) ⇒ (x - 1, y - 1)
          case (x, y) if (y > v2)            ⇒ (x, y - 1)
          case (x, y) if (x > v2)            ⇒ (x - 1, y)
          case edge                          ⇒ edge
        }
        // Remove loops
        .filterNot { edge ⇒ edge._1 == edge._2 }
      // construct a new graph and iterate
      ranomizedContraction(Graph(newVertices, newEdges))
    }

  def countCuts[T](graph: Graph[T]): Int =
    graph.edges count {
      case (0, 1) ⇒ true
      case _      ⇒ false
    }

  val fileName = if (args.isEmpty) "data.txt" else args.head
  val input = loadGraph(fileName)

  // number of iterations should be n^2 ln(n) where n is number of vertices
  val n = input.vertices.size
  val iterations = (Math.pow(n, 2) * Math.log(n)).toInt

  var minCuts = Int.MaxValue
  println(s"Looking for min cut in {$iterations} iterations")
  for (iter ← 0 until iterations) {
    if (iter % 100 == 0) {
      println(s"Iteration #{$iter}...")
    }
    implicit val rng = new Random()
    val result = ranomizedContraction(input)
    val cuts = countCuts(result)
    if (cuts < minCuts) {
      minCuts = cuts
      println(s"Found fewer cuts (${minCuts}) in iteration #{$iter}")
    }
  }
  println(s"Min cuts = ${minCuts} after {$iterations} iterations")
}

