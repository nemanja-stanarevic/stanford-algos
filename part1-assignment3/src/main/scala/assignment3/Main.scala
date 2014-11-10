package assignment3

import scala.collection.immutable._
import scala.util.Random
import scala.annotation.tailrec
import java.lang.Boolean

object Main extends App {
  case class Graph(
    vertices: Vector[Set[Int]],
    edges: Vector[(Int, Int)])

  def loadGraph(fileName: String): Graph =
    parseGraph(scala.io.Source.fromFile(fileName).mkString)

  def parseGraph(graphStr: String): Graph =
    graphStr
      .lines
      .foldLeft(Graph(Vector(), Vector())) {
        (graph, inputLine) ⇒
          val list = inputLine.split(Array(' ', '\t', ',')).foldLeft(Vector[Int]()) {
            (v, vertexStr) ⇒ v :+ vertexStr.toInt
          }
          val vertex = list.head
          val adjecency = list.tail
          val edges = adjecency map { destVertex ⇒ (vertex, destVertex) }
          Graph(
            graph.vertices :+ Set(vertex),
            graph.edges ++ edges)
      }

  @tailrec def ranomizedContraction(graph: Graph)(implicit rng: Random): Graph =
    if (graph.vertices.size <= 2)
      graph
    else {
      // pick an edge at random
      val (v1, v2) = graph.edges(rng.nextInt(graph.edges.size))
      val v1index = graph.vertices indexWhere (_ contains v1)
      val v2index = graph.vertices indexWhere (_ contains v2)
      // contract v1 and v2 into a single vertex
      val contractedVertex = graph.vertices(v1index) ++ graph.vertices(v2index)
      val newVertices = graph.vertices
        .updated(v1index, contractedVertex)
        .patch(from = v2index, patch = Nil, replaced = 1)
      // Filter out any loops
      val newEdges = graph.edges.filterNot {
        case (from, to) ⇒
          (contractedVertex contains from) && (contractedVertex contains to)
      }
      // construct a new graph and iterate
      ranomizedContraction(Graph(newVertices, newEdges))
    }

  def countCuts[T](graph: Graph): Int =
    graph.edges count {
      case (x, y) ⇒ (graph.vertices.head contains x) & !(graph.vertices.head contains y)
    }

  def calculateMinCuts(graph: Graph, debug: Boolean = false): Int = {
    val n = graph.vertices.size
    val iterations = (Math.pow(n, 2) * Math.log(n)).toInt
    if (debug)
      println(s"Looking for min cut in {$iterations} iterations")

    implicit val rng = new Random()
    var minCuts = Int.MaxValue
    for (iter ← 0 until iterations) {
      rng.setSeed(iter)
      if (debug & (iter % 100 == 0))
        println(s"Iteration #{$iter}...")
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

