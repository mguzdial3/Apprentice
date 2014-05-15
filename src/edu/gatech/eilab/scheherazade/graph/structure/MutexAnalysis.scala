package edu.gatech.eilab.scheherazade.graph.structure

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object MutexAnalysis {

  def main(args: Array[String]) {
    //    val graph = SampleGraph.sample4
    //    graph.draw("unit-analysis")
    //    val keepNode = graph.nodes.find(_.name == "C4").get
    //    val (keep, delete) = impliedGroups(keepNode, graph)
    //    println("delete = " + delete.map(_.name).mkString)
    //    println("keep = " + keep.map(_.name).mkString)
    for (i <- 0 to 9) {
      val graph = SampleGraph.randomDAG(15, 50, 5);
      graph.draw("random" + i)
    }
  }

  /**
   * if all direct predecessors of an event is in the event list, add that event to the event list
   * continue adding events until no such event exists
   */
  def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
    {

      var all = ListBuffer[Cluster]() ++ events
      var newFound: ListBuffer[Cluster] = null
      var remainder = graph.nodes filterNot (all contains)
      do {
        newFound = ListBuffer[Cluster]()
        for (e <- remainder) {
          val pred = graph.predecessorsOf(e)
          if ((!pred.isEmpty) &&
            pred.forall(all contains))
            newFound += e
        }
        all ++= newFound
        remainder = remainder filterNot (newFound contains)
      } while (!newFound.isEmpty)

      all.toList
    }

  def cleanNodes(graph: Graph, kept: List[Cluster], clan: List[EventGroup] = Nil): Graph =
    {
      var removedNodes = List[Cluster]()
      val realClans =
        if (clan == Nil) {
          UnitAnalysis.findClans(graph)
        } else {
          clan
        }

      println("rc " + realClans)

      for (me <- graph.mutualExcls) {
        if (kept.contains(me.c1) && !kept.contains(me.c2)) {
          removedNodes = me.c2 :: removedNodes
        } else if (kept.contains(me.c2) && !kept.contains(me.c1)) {
          removedNodes = me.c1 :: removedNodes
        }
      }
      //println("removed: " + removedNodes.map(_.name).mkString)

      for (c <- realClans) {
        val clan = c.nodes
        if (clan.exists(removedNodes.contains)) {
          removedNodes = clan ::: removedNodes
        }
      }
      
      removedNodes = findTransitiveClosure(graph, removedNodes)
      
      removedNodes = removedNodes.distinct
      //println("removed: " + removedNodes.map(_.name).mkString)

      if (removedNodes != Nil) {
        val cleanedGraph = graph.detectAndAddSkipLinks(removedNodes).removeNodes(removedNodes) //.graphWithOptionals 
        // we should NOT re-check optionality because of deleted events! we must keep the original optional events!

        cleanedGraph
      } else {
        graph
      }
    }

  /**
   * removes clusters that are mutually exclusive with a given list of clusters
   *
   */
  def cleanedGraph(graph: Graph, keep: List[Cluster]): Graph =
    {
      // init
      var keptNodes = keep
      var oneMoreLoop = true

      // remove as many nodes as we can
      var cleanGraph = cleanNodes(graph, keptNodes)

      var i = 0
      cleanGraph.draw("clean" + i)
      while (oneMoreLoop) {
        var newKept = keptNodes
        oneMoreLoop = false

        val clans = UnitAnalysis.findClans(cleanGraph)
        // are there more nodes that we can be sure to keep?
        for (n <- keptNodes) {
          for (clan <- clans) {
            if (clan.contains(n) && clan.nodes.filterNot(newKept.contains) != Nil) {
              newKept = clan.nodes ::: newKept // Yes. more nodes!
              oneMoreLoop = true
            }
          }
        }

        if (oneMoreLoop) {
          keptNodes = newKept.distinct
          cleanGraph = cleanNodes(graph, keptNodes, clans)
          i += 1
          cleanGraph.draw("clean" + i)
        }
      }

      // remove the START and the END, and put them back so they are behave properly 
      val start = cleanGraph.nodes.find(_.name == "START").get
      val end = cleanGraph.nodes.find(_.name == "END").get

      val g1 = new Graph(cleanGraph.nodes.filterNot(v => v == start || v == end), // remove start and ends
        cleanGraph.links.filterNot(l => l.source == start || l.target == end),
        cleanGraph.mutualExcls,
        cleanGraph.optionals, //.filterNot(n => !cleanGraph.mutualExcls.exists(me => me.c1 == n || me.c2 == n)), // remove optional and conditionals that are no longer valid
        cleanGraph.conditionals.filterNot(n => !cleanGraph.mutualExcls.exists(me => me.c1 == n || me.c2 == n)))

      AnalysisMain.addStartEnd(g1)

      // remove optional and conditional events that are no longer involved in any mutual exclusion relations

    }

  /**
   * this marker passing is incorrect. Need to label parents of kept clusters and pass markers from there, too
   *  TODO!!!!
   */
  def impliedGroups(c1: Cluster, graph: Graph): (List[Cluster], List[Cluster]) =
    {
      val topoSort = graph.topoSortInt
      val adjList = graph.getAdjacencyList

      // find the indices for c1
      var idx1 = graph.nodes.indexOf(c1)

      // states that are mutually exclusive with c1
      val deletedNodes = graph.mutualExcls.collect {
        case me: MutualExcl if (me.c1 == c1) => me.c2
        case me: MutualExcl if (me.c2 == c1) => me.c1
      }

      val deleted = deletedNodes.map(graph.nodes.indexOf(_))

      println("delete starting with " + deletedNodes.mkString)
      var (keepList, deleteList) = propagateForward(idx1, deleted, adjList, topoSort)
      (keepList.map(graph.nodes(_)), deleteList.map(graph.nodes(_)))
    }

  def propagateForward(keepNode: Int, deleteNodes: List[Int], adjList: Array[Array[Int]], topoSort: List[Int]): (List[Int], List[Int]) = {

    val keepMap = HashMap[Int, String]() // this is not a boolean value. Three possible options: unknown, must be kept, must be deleted
    for (cur <- topoSort) {
      var old = keepMap.getOrElse(cur, "")
      if (cur == keepNode) {
        old = "k"
        keepMap.update(cur, old)
      } else if (deleteNodes.contains(cur)) {
        old = "d" // it will be deleted even if its parent may be kept.
        keepMap.update(cur, old)
      }

      for (next <- adjList(cur)) {
        if (old == "k") {
          keepMap.update(next, "k") // if its parent is kept, it might be kept (not guaranteed), which is what "k" means
        } else {
          val existing = keepMap.getOrElse(next, "")
          if (existing != "k" && old == "d") {
            keepMap.update(next, "d")
          }
        }
      }
    }

    val allDelete = keepMap.filter(_._2 == "d").map(_._1).toList
    val allKeep = keepMap.filter(_._2 == "k").map(_._1).toList
    (allKeep, allDelete)
  }

  //TODO: If node A is kept, and node A has only one (non-optional) parent, the parent must be kept.
  // If A has two parents, who always happen together, they must both happen as well. Thus, we need detection of always-happen-togethers
}