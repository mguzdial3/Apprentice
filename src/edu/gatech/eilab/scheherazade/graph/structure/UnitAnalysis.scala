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
    val graph = SampleGraph.sample2
    val resultForward = propagateForward(graph)
    val c1 = findClosure(resultForward)
    val resultBackward = propagateBackward(graph)
    val c2 = findClosure(resultBackward)

    println(c1.map(x => (x._1.name + " " + x._2.map(_.name))))
    println(c2.map(x => (x._1.name + " " + x._2.map(_.name))))

    val allClosure = aggregateClosure(c1, c2)
    println(allClosure.map(_.map(_.name).mkString(", ")).mkString("\n"))
  }

  def aggregateClosure(forward: Map[Cluster, List[Cluster]], backward: Map[Cluster, List[Cluster]]): List[List[Cluster]] =
    {
      val answer = ListBuffer[List[Cluster]]()

      for (fc <- forward) {
        val ending = fc._1
        val set = fc._2

        println("ending = " + ending.name)
        println("set = " + set.map(_.name).mkString)
        
        val pass = set.exists { e =>
          val remaining = ending :: set.filterNot(_ == e)
          backward.contains(e) && remaining.filterNot(backward(e) contains) == Nil
        }

        if (pass) {
        	answer += ending :: set
        }
      }
      
      answer.toList
    }

  def propagateForward(graph: Graph): Map[Cluster, List[UnitLabel]] =
    {
      val labelsMap = HashMap[Cluster, List[UnitLabel]]()

      var queue = ListBuffer[Cluster]()
      val sources = graph.findSources
      for (s <- sources) {
        queue += s
        labelsMap.put(s, Nil)
      }

      while (!queue.isEmpty) {
        // take the first element
        val curNode = queue.head
        queue = queue.tail

        println("visiting " + curNode.name)

        val outgoing = graph.links.filter(l => l.source == curNode).distinct

        //println(outgoing.mkString("", "\n", "\n\n"))
        val count = outgoing.size
        var i = 1
        outgoing foreach {
          l =>
            // enqueue           
            val target = l.target
            val label = UnitLabel(curNode, target, count, i)
            //println("new label = " + label)
            var existingLabels = labelsMap.getOrElse(target, Nil)

            var oldLabels = labelsMap(curNode)
            if (count > 1) {
              oldLabels = split(oldLabels, count, i)
            }

            //val allLabels = 
            labelsMap.put(target, label :: label :: oldLabels ::: existingLabels)

            queue = queue.filterNot(_ == target) += target
            i += 1
        }

      }

      labelsMap.toMap
    }

  def propagateBackward(graph: Graph): Map[Cluster, List[UnitLabel]] =
    {
      val labelsMap = HashMap[Cluster, List[UnitLabel]]()

      var queue = ListBuffer[Cluster]()
      val ends = graph.findEnds
      for (s <- ends) {
        queue += s
        labelsMap.put(s, Nil)
      }

      while (!queue.isEmpty) {
        // take the first element
        val curNode = queue.head
        queue = queue.tail

        val outgoing = graph.links.filter(l => l.target == curNode)
        val count = outgoing.size
        var i = 1
        outgoing foreach {
          l =>
            // enqueue           
            val target = l.source
            val label = UnitLabel(curNode, target, count, i)

            var existingLabels = labelsMap.getOrElse(target, Nil)
            var oldLabels = labelsMap(curNode)
            if (count > 1) {
              oldLabels = split(oldLabels, count, i)
            }

            labelsMap.put(target, label :: label :: oldLabels ::: existingLabels)

            queue = queue.filterNot(_ == target) += target
            i += 1
        }

      }

      labelsMap.toMap
    }

  def split(oldLabels: List[UnitLabel], total: Int, position: Int): List[UnitLabel] =
    {
      oldLabels.map {
        label =>
          val oldSplit = label.split
          val oldPosition = label.inSplit

          val newSplit = oldSplit * total
          val newPosition = (oldPosition - 1) * total + position

          UnitLabel(label.source, label.target, newSplit, newPosition)
      }
    }

  def findClosure(labelsMap: Map[Cluster, List[UnitLabel]]): Map[Cluster, List[Cluster]] =
    {
      val closureMap = HashMap[Cluster, List[Cluster]]()
      for (pairs <- labelsMap) {
        val key = pairs._1
        val labels = pairs._2.distinct

        //println("visiting " + key.name)
        val a = labels.groupBy(_.source)
        //println(a)
        val k = a.map(x => (x._1, x._2.size / x._2.head.split)).filter(x => x._2 > 0).map(_._1).toList
        closureMap.put(key, k)
      }
      //      val str = closureMap.map(x => (x._1, x._2.map(_.name))).mkString("\n")
      //      println(str)

      closureMap.filter(_._2 != Nil).toMap
    }

}

case class UnitLabel(val source: Cluster, val target: Cluster, val split: Int, val inSplit: Int) {
  override def toString() =
    source.name + " -> " + target.name + " " + inSplit + "/" + split
}