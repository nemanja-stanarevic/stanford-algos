package assignment1

import scala.collection.immutable._
import scala.annotation.tailrec

object Main1 extends App {
  case class Job(weight: Long, length: Long)
  case class CompletedJob(weight: Long, completion: Long)

  def loadData(fileName: String): Vector[Job] =
    parseData(scala.io.Source.fromFile(fileName).getLines)

  def parseData(lines: Iterator[String]): Vector[Job] =
    lines
      .drop(1)    // skip the line with number of jobs
      .foldLeft(Vector[Job]()) {
        (jobs, inputLine) ⇒
          val job = inputLine.split(Array(' ', '\t'))
          val weight = job.head.toLong
          val length = job.tail.head.toLong
          jobs :+ Job(weight, length)
      }
  val fileName = if (args.isEmpty) "data-1.txt" else args.head
  var input = loadData(fileName)
 
  def toCompletedJobs(jobs: Vector[Job]) : Vector[CompletedJob] = 
    jobs
      .foldLeft(Vector[CompletedJob]()) { (completedJobs, job) =>
        if (completedJobs.isEmpty) 
          completedJobs :+ CompletedJob(job.weight, job.length)
        else
          completedJobs :+ CompletedJob(job.weight, job.length + completedJobs.last.completion)
      }
  
  def weightedCompletion(jobs: Vector[Job]): Long =
    toCompletedJobs(jobs)
      .foldLeft(0l) {
        (sum, job) => sum + job.weight * job.completion
      }

  def differenceLt(a: Job, b: Job): Boolean = {
    val aMeasure = a.weight - a.length
    val bMeasure = b.weight - b.length
    if (aMeasure == bMeasure)
      a.weight < b.weight
    else
      (aMeasure < bMeasure)
  }
  
  def ratioLt(a: Job, b: Job): Boolean = {
    val aMeasure = 1.0 * a.weight / a.length
    val bMeasure = 1.0 * b.weight / b.length
    (aMeasure < bMeasure)
  }
  
  val result1 = weightedCompletion(input.sortWith(differenceLt).reverse)
  val result2 = weightedCompletion(input.sortWith(ratioLt).reverse)
  println(s"Problem 1) solution = ${result1}.")
  println(s"Problem 2) solution = ${result2}.")
}

