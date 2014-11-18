package assignment5

import scala.collection.immutable._
import scala.util.Random
import scala.annotation.tailrec
import java.lang.Boolean
import scala.collection.generic.Sorted

object Main extends App {
  type Label = Int
  type Length = Int
  case class DirectedGraph(
    vertices: HashSet[Label],
    adjecency: HashMap[Label, Vector[(Label, Length)]])

  val infiniteLength = 1000000

  // helper function used with HashMap's applyOrElse
  def emptyVector(l: Label) = Vector.empty[Label]

  def loadGraph(fileName: String): DirectedGraph =
    parseGraph(scala.io.Source.fromFile(fileName).getLines)

  def parseGraph(lines: Iterator[String]): DirectedGraph =
    lines
      .foldLeft(DirectedGraph(HashSet(), HashMap())) {
        (graph, inputLine) ⇒
          val list = inputLine
            .split(Array(' ', '\t'))
            .foldLeft(Vector[String]()) { (v, vertexStr) ⇒ v :+ vertexStr }
          val from = list.head.toInt
          val toWithLength = list.tail
            .foldLeft(Vector[(Label, Length)]()) { (v, str) ⇒
              v :+ (str.split(',').head.toInt, str.split(',').tail.head.toInt)
            }
          DirectedGraph(
            graph.vertices + from ++ (toWithLength.map{ case (v, _) ⇒ v }),
            graph.adjecency.updated(from, toWithLength))
      }

  // DFS to id vertices that are disconnected from the starting vertex in G
  def dfs(graph: DirectedGraph, i: Label): HashSet[Label] = {
    var explored = HashSet[Label]()
    def dfsRec(i: Label): Unit = {
      explored = explored + i
      graph
        .adjecency(i)
        .foreach {case (j, l) =>
          if ( !(explored contains j) )
            dfsRec(j)
      }
    }
    dfsRec(i)
    explored
  }

  def dijkstra(graph: DirectedGraph, s: Label): HashMap[Label, Length] = {
    // id disconnected vertices, if any
    val connectedVertices = dfs(graph, s)
    val disconnectedVertices = graph.vertices -- connectedVertices
    // fill in "infinite" length edges between the starting vertex and each
    // disconnected vertex
    val connectedGraph = DirectedGraph(
        graph.vertices,
        graph.adjecency.updated(s, graph.adjecency(s) ++ (disconnectedVertices map { v => (v, infiniteLength) })))

    @tailrec def dijkstraRec(explored: HashSet[Label], lengths: HashMap[Label, Length]) : HashMap[Label, Length] = 
      explored match {
        case allExplored if allExplored == connectedGraph.vertices =>
          lengths
        case explored =>
          val unexplored = connectedGraph.vertices -- explored
          val candidateEdges = connectedGraph.adjecency
            .toVector
            .filter { case (from, tos) => (explored contains from) }
            .flatMap { case (from, tos) => 
              tos
                .filter { case (to, length) => (unexplored contains to) }
                .map{ case (to, length) =>  (from, to, length) }
            }
          val (from, to, length) = candidateEdges
            .minBy[Int] {
              case (from, to, length) =>
                lengths(from) + length
            }
          dijkstraRec(
            explored + to, 
            lengths + (to -> (lengths(from) + length)))
      }

    dijkstraRec(HashSet(s), HashMap(s -> 0))
  }

  val fileName = if (args.isEmpty) "data.txt" else args.head
  var graph = loadGraph(fileName)
  val shortestPaths = dijkstra(graph, 1)
  // pick the desired results
  val desiredVertices = Set(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
  val result = shortestPaths
    .toVector
    .collect { case (vertex, length) if desiredVertices contains vertex ⇒ (vertex, length) }
    .sorted( Ordering.by[(Label, Length), Int] { case (vertex, length) => vertex } )
    .map { case (vertex, length) => length}
    .mkString(",")

  println(s"result = $result")
}

