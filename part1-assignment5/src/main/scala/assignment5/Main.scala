package assignment5

import scala.collection.immutable._
import scala.util.Random
import scala.annotation.tailrec
import java.lang.Boolean

object Main extends App {
  type Label = Int
  case class DirectedGraph(
    vertices: HashSet[Label],
    adjecency: HashMap[Label, Vector[Label]],
    incidence: HashMap[Label, Vector[Label]])

  // helper function used with HashMap's applyOrElse
  def emptyVector(l: Label) = Vector.empty[Label]

  def loadGraph(fileName: String): DirectedGraph =
    parseGraph(scala.io.Source.fromFile(fileName).getLines)

  def parseGraph(lines: Iterator[String]): DirectedGraph =
    lines
      .foldLeft(DirectedGraph(HashSet(), HashMap(), HashMap())) {
        (graph, inputLine) ⇒
          val edge = inputLine.split(Array(' ', '\t', ','))
          val fromV = edge.head.toInt
          val toV = edge.tail.head.toInt
          DirectedGraph(
            (graph.vertices + fromV) + toV,
            graph.adjecency.updated(fromV, graph.adjecency.applyOrElse(fromV, emptyVector) :+ toV),
            graph.incidence.updated(toV, graph.incidence.applyOrElse(toV, emptyVector) :+ fromV))
      }

  def kosaraju(graph: DirectedGraph): HashMap[Label, Vector[Label]] = {
    var time = 0
    var explored = HashSet[Label]()
    var ordering = HashMap[Label, Int]()
    var leader = HashMap[Label, Vector[Label]]()
    var source: Option[Label] = None

    // this is a simple helper to return updated leaders. 
    // Slight modification from class notes, here leader is a map of 
    // v -> vector(vs) where vertex v is a leader for all vertices vs
    def leaderUpdated(i: Label): HashMap[Label, Vector[Label]] = source match {
      case Some(src) ⇒ leader.updated(src, leader.applyOrElse(src, emptyVector) :+ i)
      case None      ⇒ leader
    }

    def dfsLoop(vertices: Vector[Label], reverse: Boolean) = {
      time = 0
      explored = HashSet()
      ordering = HashMap()
      leader = HashMap()
      source = None

      // this is a simple helper that returns arcs from a given vertex 
      // either in reverse or forward direction (closes over reverse parameter 
      // and graph). If no arcs, returns an empty vector
      def getArcs(vertex: Label): Vector[Label] =
        if (!reverse) graph.adjecency.applyOrElse(vertex, emptyVector)
        else graph.incidence.applyOrElse(vertex, emptyVector)

      // recursive solution as described in the class notes is very 
      // nice & elegant, but blows up a really big stack really fast
      // It is not actually used in dfsLoop
      def dfsRecursive(i: Label): Unit = {
        explored = explored + i
        leader = leaderUpdated(i)
        getArcs(i)
          .foreach { j ⇒
            if (!(explored contains j)) {
              dfsRecursive(j)
            }
          }
        time = time + 1
        ordering = ordering.updated(i, time)
      }

      // tail recursive version of dfs
      // not as nice, but keeps its stack together
      def dfsTail(i: Label) = {
        dfs(i :: Nil)

        @tailrec def dfs(stack: List[Label]): Unit = stack match {
          case head :: tail ⇒
            if (!(explored contains head)) {
              explored = explored + head
              leader = leaderUpdated(head)
              getArcs(head)
                .toList
                .filter { j ⇒ !(explored contains j) } 
                 match {
                   case Nil ⇒
                     time = time + 1
                     ordering = ordering.updated(head, time)
                     dfs(tail)
                   case pathsToTake ⇒
                     dfs(pathsToTake ::: (head :: tail))
                 }
            } else {
              if (!(ordering contains head)) {
                time = time + 1
                ordering = ordering.updated(head, time)
              }
              dfs(tail)
            }
          case Nil ⇒
          // do nothing, backtrack
        }
      }

      vertices.foreach { i ⇒
        if (!(explored contains i)) {
          source = Some(i)
          dfsTail(i)
        }
      }
    }

    dfsLoop(graph.vertices.toVector, reverse = true)
    // sort in linear time (n) & reverse order at the same time
    var sorted = Vector.fill(graph.vertices.size)(0)
    for (vertex ← 1 to graph.vertices.size)
      sorted = sorted.updated(graph.vertices.size - ordering(vertex), vertex)
    dfsLoop(sorted, reverse = false)

    leader
  }

  val fileName = if (args.isEmpty) "data.txt" else args.head
  var graph = loadGraph(fileName)
  val leader = kosaraju(graph)
  // pick the top 5 SCCs
  // results are okay to sort in nlogn time since it's a list of SCCs (<< m,n)
  val top5 = leader.values
      .map { vect ⇒ vect.size }
      .toVector
      .sorted(scala.math.Ordering[Label].reverse)
      .take(5)

  println(s"Top 5 SCCs: ${top5}")
}

