package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
/**
 * This code finds independent units in plot graphs.
 *  An independent unit is like
 *     *
 *    /|\
 *   * * *
 *    \|/
 *     *
 *  If an independent unit does not have mutual exclusion with outside vertices, it can be treated as one unit that appears and disappears together from outside.
 */
object UnitAnalysis {

  def main(args: Array[String]) {
    val graph = SampleGraph.randomDAG(10, 15)
    graph.draw("unit-analysis")
    val lvlMap = assignLevel(graph)
    lvlMap.foreach(
      p =>
        println(p._1.name + " : " + p._2))
  }

  def assignLevel(graph: Graph): Map[Cluster, Int] =
    {
      val lvlMap = HashMap[Cluster, Int]()
      var queue = ListBuffer[(Cluster, Int)]()
      val sources = graph.findSources
      for (s <- sources) {
        queue += ((s, 0))
      }

      while (!queue.isEmpty) {
        // take the first element
        val head = queue.head
        val curCount = head._2
        val curNode = head._1
        queue = queue.tail

        graph.links.filter(l => l.source == curNode) foreach {
          l =>
            // enqueue
            val target = l.target
            queue = queue.filterNot(elem => elem._1 == target) += ((target, curCount + 1))
        }

        if (lvlMap.contains(curNode)) {
          val oldCount = lvlMap(curNode)
          if (oldCount > curCount) {
            println("error: old count > cur count " + curNode.name)
          }
        }
        lvlMap.put(curNode, curCount)
      }

      lvlMap.toMap
    }
}

class UnitLabel(val init: Cluster, val initLowerBound: Int, val initUpperBound: Int, val split: Int, val inSplit: Int)