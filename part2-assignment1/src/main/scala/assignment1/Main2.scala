package assignment1

import scala.collection.immutable._
import scala.util.Random
import scala.annotation.tailrec
import java.lang.Boolean
import scala.collection.generic.Sorted

object Main2 extends App {
  type Label = Long
  type Cost = Long
  case class Graph(
    vertices: HashSet[Label],
    adjecency: Vector[(Label, Label, Cost)])

  def loadGraph(fileName: String): Graph =
    parseGraph(scala.io.Source.fromFile(fileName).getLines)

  def parseGraph(lines: Iterator[String]): Graph =
    lines
      .drop(1)
      .foldLeft(Graph(HashSet(), Vector())) {
        (graph, inputLine) ⇒
          val list = inputLine
            .split(Array(' ', '\t'))

          val from = list(0).toLong
          val to = list(1).toLong
          val cost = list(2).toLong

          Graph(
            graph.vertices + from + to,
            graph.adjecency :+ (from, to, cost)
          )
      }

  def prim(graph: Graph): Graph = {
    @tailrec def primRec(spanningTree: Graph): Graph = {
        if (spanningTree.vertices != graph.vertices) {
          val nextEdge = graph
            .adjecency
            .filter { case (from, to, _) =>
              (spanningTree.vertices.contains(from) && !spanningTree.vertices.contains(to)) || 
              (!spanningTree.vertices.contains(from) && spanningTree.vertices.contains(to))
            }
            .minBy { case (from, to, cost) => cost }
          primRec(Graph(spanningTree.vertices + nextEdge._1 + nextEdge._2, spanningTree.adjecency :+ nextEdge))
        } else {
          spanningTree
        }
    }
    primRec(Graph(HashSet[Label](graph.vertices.head), Vector[(Label, Label, Cost)]()))
  }

  val fileName = if (args.isEmpty) "data-2.txt" else args.head
  val graph = loadGraph(fileName)
  val spanningTree = prim(graph)

  val result = spanningTree.adjecency
    .map { case(from, to, cost) => cost }
    .sum

  println(s"result = $result")
}

