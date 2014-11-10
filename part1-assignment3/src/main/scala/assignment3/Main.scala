package assignment3

import scala.collection.immutable._
import scala.util.Random
import scala.annotation.tailrec
import java.lang.Boolean

object Main extends App {
  case class Graph[T](
    vertices: Vector[T],
    edges: Vector[(Int, Int)])

  def loadGraph(fileName: String): Graph[Set[Int]] =
    parseGraph(scala.io.Source.fromFile(fileName).mkString)

  def parseGraph(graphStr: String): Graph[Set[Int]] = 
    graphStr
      .lines
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
      val (v1, v2) = graph.edges(rng.nextInt(graph.edges.size))
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

  def calculateMinCuts(graph: Graph[Set[Int]], debug: Boolean = false): Int = {
    val n = graph.vertices.size
    val iterations = ( Math.pow(n, 2) * Math.log(n) ).toInt
    if (debug)
      println(s"Looking for min cut in {$iterations} iterations")

    var minCuts = Int.MaxValue
    for (iter <- 0 until iterations) {
      if (debug & (iter % 100 == 0))
        println(s"Iteration #{$iter}...")

      implicit val rng = new Random()
      val cuts = countCuts(ranomizedContraction(graph))
      if (cuts < minCuts) {
        minCuts = cuts
        if (debug)
          println(s"Found fewer cuts (${minCuts}) in iteration #{$iter}")
      }
    }
    minCuts
  }

  val fileName = if (args.isEmpty) "data.txt" else args.head
  val input = loadGraph(fileName)
  val minCuts = calculateMinCuts(input, debug = true)
  println(s"Min cuts = ${minCuts}")
}

