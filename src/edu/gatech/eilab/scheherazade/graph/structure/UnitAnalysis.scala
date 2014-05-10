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
    val graph = SampleGraph.sample5
    graph.draw("unit-analysis")
    val clans = findClans(graph)
    //println(clans.mkString(", "))
  }

  def analyzeUnit(graph: Graph): (List[EventGroup], List[EventGroup]) =
    {
      val forwardSort = graph.topoSortInt
      val backwardSort = forwardSort.reverse

      val (forwardAdjList, backwardAdjList) = graph.getBiAdjacencyList

      val resultForward = propagate(graph, forwardSort, forwardAdjList)
      val c1 = findClosure(resultForward)
      val resultBackward = propagate(graph, backwardSort, backwardAdjList)
      val c2 = findClosure(resultBackward)

      println("c1 :" + c1.map(x => (x._1.name + " " + x._2.map(_.name))))
      println("c2 :" + c2.map(x => (x._1.name + " " + x._2.map(_.name))))
      //    println(resultForward.map(x => (x._1.name + " " + x._2.mkString(",   "))).mkString("\n"))
      //    println(resultBackward.map(x => (x._1.name + " " + x._2.mkString(",   "))).mkString("\n"))

      val allGroups = aggregateClosure(c1, c2)
      println("all groups: " + allGroups.mkString(", "))
      // those that always happen together
      val clans = allGroups.filter(testAlwaysTogether(_, graph))
      // those that are independent of others
      val closures = allGroups.filter(testClosureMutex(_, graph))
      (clans, closures)

    }

  def findClans(graph: Graph): List[EventGroup] =
    {
      val nodes = graph.nodes
      val mutexList = graph.mutualExcls

      val mergeList = ListBuffer[EventGroup]()

      val (kidList, parentList) = graph.getBiAdjacencyList()
      var toposort = graph.topoSortInt()

      while (toposort != Nil) {
        val cur = toposort.head
        val curNode = graph.nodes(cur)
        toposort = toposort.tail
        var curMergeList = mergeList.find(_ contains curNode).getOrElse(new EventGroup(curNode))
        mergeList -= curMergeList

        for (kid <- kidList(cur)) {
          val kidNode = nodes(kid)
          // merge conditions:
          // 1. all of its parent are in the mergeList 
          // 2. it is not involved in any mutual exclusion relations
          if (parentList(kid).filterNot(i => curMergeList.contains(graph.nodes(i))).size == 0 && !mutexList.exists(me => me.c1 == kidNode || me.c2 == kidNode)) {
            // merge it with its parent
            curMergeList = curMergeList.append(graph.nodes(kid))
          }
        }

        if (curMergeList.size > 1) {
          mergeList += curMergeList
        }
      }

      mergeList.toList
    }

  def testAlwaysTogether(closure: EventGroup, graph: Graph) = {
    val all = closure.nodes
    // middles and ends are not involved in any mutual exclusions
    (closure.end :: closure.middle).forall(m =>
      !graph.mutualExcls.exists(me =>
        (me.c1 == m || me.c2 == m)))
  }

  def testClosureMutex(closure: EventGroup, graph: Graph) = {
    val all = closure.nodes
    // middles and ends are not involved in external mutual exclusions
    (closure.end :: closure.middle).forall(m =>
      !graph.mutualExcls.exists(me =>
        (me.c1 == m && (!all.contains(me.c2))) ||
          (me.c2 == m && (!all.contains(me.c1))))) &&
      // the end is either optional or not involved in any mutex
      (graph.optionals.contains(closure.end) ||
        !graph.mutualExcls.exists(me =>
          (me.c1 == closure.end || me.c2 == closure.end)))
    // the head can be in any mutex
  }

  def aggregateClosure(forward: Map[Cluster, List[Cluster]], backward: Map[Cluster, List[Cluster]]): List[EventGroup] =
    {
      val answer = ListBuffer[EventGroup]()

      for (fc <- forward) {
        val ending = fc._1
        val set = fc._2
        var start: Cluster = null

        //        println("ending = " + ending.name)
        //        println("set = " + set.map(_.name).mkString)

        for (backPair <- backward) {
          val backKey = backPair._1
          val backList = backPair._2

          if (set.contains(backKey)) {
            val remaining = set.filterNot(_ == backKey)
            if (remaining.filterNot(backList contains) == Nil) {
              // this is a unit structure
              answer += EventGroup(backKey, ending, set filterNot (_ == backKey))
            }
          }
        }

      }

      answer.toList
    }

  def propagate(graph: Graph, topoSort: List[Int], adjacentList: Array[Array[Int]]): Map[Cluster, List[UnitLabel]] =
    {
      val labelsMap = HashMap[Int, List[UnitLabel]]()
      val nodes = graph.nodes.toArray

      for (v <- topoSort) {
        val oldLabels = labelsMap.getOrElse(v, Nil)
        val n = adjacentList(v).size
        //        print(v + " from "+ nodes(v).name)
        for (i <- 0 until n) {
          // split the oldLabels
          val splitLabels = split(oldLabels, n, i + 1)

          val kidId = adjacentList(v)(i)
          //          println(" to "+ nodes(kidId).name + "... split = " + (i+1) + "/" + n)
          //          println(oldLabels)
          val label = UnitLabel(nodes(v), nodes(kidId), n, i + 1)
          val existingLabels = labelsMap.getOrElse(kidId, Nil)
          val allLabels = label :: splitLabels ::: existingLabels
          labelsMap.put(kidId, allLabels)
        }
      }

      labelsMap.map(pair => (nodes(pair._1), pair._2)).toMap
    }

  private def split(oldLabels: List[UnitLabel], total: Int, position: Int): List[UnitLabel] =
    {
      oldLabels.map {
        label =>
          val oldSplit = label.split
          val oldPosition = label.inSplit

          val newSplit = oldSplit * total
          val newPosition = (oldPosition - 1) * total + position
          //          println(" new split = * " + total + " " + newSplit)
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
        val k = a.map(x => (x._1, x._2.map(label => 1.0 / label.split).sum)).filter(x => x._2 > 0.9999).map(_._1).toList
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

//case class Closure(val start: Cluster, val end: Cluster, val middle: List[Cluster]) {
//  override def toString() =
//    "Closure (" + start.name + "-" + end.name + " : " + middle.map(_.name).mkString(",") + ")"
//  def nodes = start :: end :: middle
//}

/**
 * an event group is a set of events
 *
 */
case class EventGroup(val start: Cluster, val end: Cluster, val middle: List[Cluster]) {
  override def toString() =
    "Group (" +
      { if (start != null) start.name else "?" } +
      "-" +
      { if (end != null) end.name else "?" } +
      " : " + middle.map(_.name).mkString(",") + ")"
  def nodes = start :: end :: middle

  /**
   * add another cluster from bottom
   *
   */
  def append(c: Cluster): EventGroup =
    {
      if (c != start && c != end && !middle.contains(c)) {
        if (end != null) {
          EventGroup(start, c, end :: middle)
        } else EventGroup(start, c, middle)
      }
      else
      {
        this
      }
    }

  def contains(c: Cluster) = start == c || end == c || middle.contains(c)

  def this(c: Cluster) = this(c, null, Nil)
  def size(): Int =
    {
      var count = 0
      if (start != null) count += 1
      if (end != null) count += 1
      count + middle.size
    }

  def toList(): List[Cluster] =
    {
      var m = middle
      if (start != null) {
        m = start :: m
      }
      if (end != null) {
        m = end :: m
      }
      m
    }

}