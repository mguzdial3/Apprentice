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
    for (i <- 0 to 9)
    {
    	val graph = SampleGraph.randomDAG(15, 50, 5);
    	graph.draw("random" + i)
    }
  }
  
  /** removes clusters that are mutually exclusive with a given list of clusters
   *  
   */
  def cleanedGraph(graph:Graph, keep:List[Cluster], clans:List[EventGroup]):Graph =
  {
    var removedNodes = ListBuffer[Cluster]()
    
//    for (keepNode <- keep)
//    {
//      removedNodes ++= impliedGroups(keepNode, graph)._2      
//    }
//    removedNodes = removedNodes.distinct
    
    for (me <- graph.mutualExcls)
    {
      if (keep.contains(me.c1) && !keep.contains(me.c2))
      {
        removedNodes += me.c2
      }
      else if (keep.contains(me.c2) && !keep.contains(me.c1))
      {
        removedNodes += me.c1
      }
    }
    println("removed: " + removedNodes.map(_.name).mkString)
    
    for (c <- clans)
    {
    	val clan = c.nodes
    	if (clan.exists(removedNodes contains))
    	{
    	  removedNodes ++= clan
    	}
    }
    removedNodes = removedNodes.distinct
    println("removed: " + removedNodes.map(_.name).mkString)
    graph.removeNodes(removedNodes.toList)
  }

  /** this marker passing is incorrect. Need to label parents of kept clusters and pass markers from there, too
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