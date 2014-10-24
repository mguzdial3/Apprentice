package edu.gatech.eilab.scheherazade

import main._
import data._
import graph._
import io._
import java.io._
import graph.passage._
import scala.collection.mutable.Queue
import edu.gatech.eilab.scheherazade.graph.sample.CfRSample

package generation {
  /**
   * exhaustively generate all sequences from a plot graph
   * We store everything in the memory, which requires the number of sequences to be small.
   */
  object ExhaustiveSeqGen {

    def generate(graph: Graph): List[List[Cluster]] =
      {
        var allSeq = List[List[Cluster]]()
        var firstWalk: AbstractPassage = Passage.init(graph)
        var queue = Queue[(AbstractPassage, List[Cluster])]()
        queue.enqueue((firstWalk, List[Cluster]()))
        while (!queue.isEmpty) {
          val top = queue.dequeue
          val walk = top._1
          val history = top._2
          if (walk.hasMoreSteps) {
            val fringe = walk.fringe
            for (step <- fringe) {
              val newWalk = walk.forward(step)
              val newHistory = step :: history
              queue.enqueue((newWalk, newHistory))
            }
          } else {
            allSeq = history.reverse :: allSeq
          }
        }

        allSeq
      }
    
  def main(args: Array[String]) {
    val graph = CfRSample.graph11
    graph.draw("exhaustive")
    val all = generate(graph)
    println(all.map(_.map(_.name).mkString(", ")).mkString("\n"))
  }
  
  
  }
  
  
}