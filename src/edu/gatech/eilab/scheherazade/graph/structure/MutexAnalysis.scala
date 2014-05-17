package edu.gatech.eilab.scheherazade.graph.structure

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
object MutexAnalysis {

  def main(args: Array[String]) {
    val graph = SampleGraph.sample12.graphWithOptionalsAndSkips
    graph.draw("special-test")
    val kept = List(graph.nodes(2))
    println(kept)
    val r = causeForDeletionNew(graph, kept)
    println("causes =" + r)
    cleanNodesNew(graph, kept)
  }

  /**
   * if all direct predecessors of an event is in the event list, add that event to the event list
   * continue adding events until no such event exists
   */
  def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
    {

      var all = ListBuffer[Cluster]() ++ events //.filterNot(e => graph.optionals.contains(e) || graph.conditionals.contains(e)) // optional or conditionals do not apply
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

  /**
   * find who is responsible for deleting what
   *
   */
  def causeForDeletion(graph: Graph, kept: List[Cluster]) = {

    val causes = HashMap[Cluster, List[Set[Cluster]]]() // the cause for deleting a node
    var deleted = List[Cluster]()

    // immediately excluded
    for (me <- graph.mutualExcls) {
      var excluded: Cluster = null
      var cause: Cluster = null
      if (kept.contains(me.c1)) {
        excluded = me.c2
        cause = me.c1
      } else if (kept.contains(me.c2)) {
        excluded = me.c1
        cause = me.c2
      }

      if (excluded != null) {
        // either c1 or c2 is an node in kept.

        //println(cause.name + " deletes " + excluded.name)

        deleted = excluded :: deleted

        if (causes.contains(excluded)) {
          // this is an OR relationship
          val oldList = causes(excluded)
          causes += (excluded -> (Set(cause) :: oldList))
        } else {
          causes += ((excluded, List(Set(cause))))
        }
      }
    }

    val toposort = graph.topoSort

    var remainder = graph.nodes filterNot (deleted contains)
    var newFound: List[Cluster] = Nil

    for (e <- toposort) {
      val pred = graph.predecessorsOf(e)
      if ((!pred.isEmpty) && pred.forall(deleted contains)) {

        if (!deleted.contains(e)) {
          deleted = e :: deleted
        }

        // needs to get causes from parents.

        // AND relation between all predecessors
        var allCauses = pred.map(causes(_))
        var comb = allCauses.head
        allCauses = allCauses.tail

        while (allCauses != Nil) {
          val next = allCauses.head
          allCauses = allCauses.tail

          comb = comb.flatMap(x => next.map(y => x ++ y))
        }

        if (causes.contains(e)) {
          val old = causes(e)
          causes += (e -> (comb ::: old).distinct) // OR relation            
        } else {
          causes += (e -> comb)
        }

      }
    }

    causes
  }

  /**
   * helper method
   *
   */
  private def addCause(map: HashMap[Cluster, List[Set[Cluster]]], cause: Cluster, excluded: Cluster) =
    {
      if (map.contains(excluded)) {
        // this is an OR relationship
        val oldList = map(excluded)
        map += (excluded -> (Set(cause) :: oldList))
      } else {
        map += ((excluded, List(Set(cause))))
      }
    }

  /**
   * finds who is responsible for deleting what
   *
   */
  def causeForDeletionNew(graph: Graph, kept: List[Cluster]) = {

    // first, exclude any nodes that must be deleted from causers
    var deleted = List[Cluster]()
    // immediately excluded by events in the kept list
    for (me <- graph.mutualExcls) {
      var excluded: Cluster = null
      var cause: Cluster = null
      if (kept.contains(me.c1)) {
        deleted = me.c2 :: deleted
      } else if (kept.contains(me.c2)) {
        deleted = me.c1 :: deleted
      }
    }

    // delete nodes by transitive closure
    val toposort = graph.topoSort
    for (e <- toposort) {
      val pred = graph.predecessorsOf(e)

      if ((!pred.isEmpty) && pred.forall(deleted contains) && !deleted.contains(e)) {
        deleted = e :: deleted
      }
    }

    deleted = deleted.distinct
    val possibleCauses = graph.nodes.filterNot(deleted.contains)

    // second, figure out what causes immediately what to be deleted

    val causes = HashMap[Cluster, List[Set[Cluster]]]() // the cause for deleting a node

    for (me <- graph.mutualExcls) {
      if (deleted.contains(me.c1)) {
        addCause(causes, me.c2, me.c1) // me.c2 is the cause
      } else if (deleted.contains(me.c2)) {
        addCause(causes, me.c1, me.c2) // me.c1 is the cause
      } else {
        addCause(causes, me.c1, me.c2)
        addCause(causes, me.c2, me.c1) // either can cause the other to be excluded
      }
    }

    // Third, propogate the causes through the graph

    for (e <- toposort) {
      // needs to get causes from parents.
      val pred = graph.predecessorsOf(e)

      // AND relation between all predecessors
      var allCauses = causes.filter(p => pred.contains(p._1)).map(_._2)

      //      if (allCauses == Nil)  {
      //        // do nothing
      //      } else if (allCauses.exists(c => c.size >= 2)) {
      //        // if any predecessors have two causes, this will have two or more causes
      //        causes += (e -> List[Set[Cluster]](Set[Cluster](), Set[Cluster]())) // two empty space holders
      //      } else {
      //        val old = causes.getOrElse(e, List(Set[Cluster]()))
      //        if (old.size >= 2) {
      //          causes += (e -> List[Set[Cluster]](Set[Cluster](), Set[Cluster]())) // two empty space holders
      //        } else {
      //          val cat = (old :: allCauses.toList).reduce((x, y) => List(x.head ++ y.head))
      //          causes += (e -> cat)
      //        }
      //      }

      allCauses = allCauses.map(item => item.filterNot(set => set.exists(n => graph.shortestDistance(n, e) != -1)))

//      println("processing : " + e)
//      println(allCauses.map(_.mkString(" OR ")).mkString(", "))

      if (allCauses != Nil) { // combine the causes from predecessors
        var comb = allCauses.head
        allCauses = allCauses.tail

        while (allCauses != Nil) {
          val next = allCauses.head.filterNot(set => set.exists(n => graph.shortestDistance(n, e) != -1))
          allCauses = allCauses.tail

          comb = comb.flatMap(x => next.map(y => x ++ y)).filterNot(s => s.size == 0)
//          println("comb " + comb)
        }

        if (causes.contains(e)) {
          val old = causes(e)
          causes += (e -> (comb ::: old).distinct) // OR relation            
        } else {
          causes += (e -> comb)
        }
      }

    }

    (causes, deleted)
  }

  //  def transitiveClosure(graph:Graph, deleted:List[Cluster], forbidden:List[Cluster])
  //  {
  //    
  //  }

  //  def findTransitiveClosureBAD(graph: Graph, events: List[Cluster]): List[Cluster] =
  //    {
  //
  //      val canSkip = events.filterNot(e => graph.optionals.contains(e) || graph.conditionals.contains(e)) // optional or conditionals do not apply
  //      val cannotSkip = events.filterNot(canSkip contains)
  //
  //      var all = ListBuffer[Cluster]() ++ cannotSkip
  //      var newFound: ListBuffer[Cluster] = null
  //      var remainder = graph.nodes filterNot (all contains)
  //      do {
  //        newFound = ListBuffer[Cluster]()
  //        for (e <- remainder) {
  //          val pred = graph.predecessorsOf(e)
  //          if ((!pred.isEmpty) &&
  //            pred.forall(all contains))
  //            newFound += e
  //        }
  //        all ++= newFound
  //        remainder = remainder filterNot (newFound contains)
  //      } while (!newFound.isEmpty)
  //
  //      all.toList ::: canSkip
  //    }

  /* a new version of clean nodes that fix the special case
   * 
   */
  def cleanNodesNew(graph: Graph, kept: List[Cluster]) =
    {
      val (causes, d) = causeForDeletionNew(graph, kept)
      var removedNodes = d
//      println("causes = " + causes)
//      println("d = " + removedNodes)
      /* Special care is needed when node C excludes node A AND
	   * node A is a predecessor to node B AND 
	   * B is parallel to C and not marked for deletion 
	  */
      val insertLink = HashMap[Cluster, List[Cluster]]()
      var dontDelete = List[Cluster]()
      for (n <- removedNodes if !graph.optionals.contains(n)) {
        // don't perform this test if n is optional
        // because an optional event is not a precondition for anything, 
        // so removing it is not removing any preconditions

        val potential = HashMap[Cluster, List[Cluster]]()

        val kids = graph.successorsOf(n)
        val causeN = causes(n)

        val parallelCauses = causeN.map { AndedCause =>
          AndedCause.filter { c =>
            var exists = false
            for (k <- kids) {
              if (!graph.ordered(c, k)) {
                exists = true
                potential += (k -> (c :: potential.getOrElse(k, Nil)))
              }
            }
            exists
          }
        }

        if (parallelCauses.size == 1) {
          insertLink ++= potential
        } else if (parallelCauses.size > 1) {
          dontDelete = n :: dontDelete
        }
      }

      removedNodes = removedNodes.filterNot(dontDelete.contains)
      insertLink --= removedNodes
      //      println("need to insert link " + insertLink)
      //      println("dont delete " + dontDelete)
      //      println("delete = " + removedNodes)

      if (removedNodes != Nil) {
        val newGraph = graph.detectAndAddSkipLinks(removedNodes).removeNodes(removedNodes)
        // we should NOT re-check optionality because of deleted events! we must keep the original optional events!
        val newLinks = insertLink.flatMap {
          case (n, list) =>
            list.map(x => new Link(x, n))
        }.toList

        new Graph(newGraph.nodes, newLinks ::: newGraph.links, newGraph.mutualExcls, newGraph.optionals, newGraph.conditionals)
      } else {
        graph
      }
    }

  /**
   * dont delete any node A who are mutually exclusive with two or more nodes
   *  and some of A's decendents are parallel to these two nodes
   *  and those two nodes are also parallel (?)
   */
  def findDontDeletes(graph: Graph): List[Cluster] =
    {
      graph.nodes.filter { n =>
        var ex = List[Cluster]()
        for (me <- graph.mutualExcls) {
          if (me.c1 == n) {
            ex = me.c2 :: ex
          } else if (me.c2 == n) {
            ex = me.c1 :: ex
          }
        }

        val kids = graph.successorsOf(n)

        ex = ex.filter(e => kids.exists(k => graph.ordered(e, k)))

        ex.size >= 2
      }

      null
    }

  //  def transClosure(graph: Graph, kept: List[Cluster], dontDelete: List[Cluster]) {
  //    // first, exclude any nodes that must be deleted from causers
  //    var deleted = List[Cluster]()
  //    // immediately excluded by events in the kept list
  //    for (me <- graph.mutualExcls) {
  //      var excluded: Cluster = null
  //      var cause: Cluster = null
  //      if (kept.contains(me.c1)) {
  //        deleted = me.c2 :: deleted
  //      } else if (kept.contains(me.c2)) {
  //        deleted = me.c1 :: deleted
  //      }
  //    }
  //
  //    deleted = deleted.filterNot(dontDelete contains).distinct
  //    
  //    // delete nodes by transitive closure
  //    val toposort = graph.topoSort
  //    for (e <- toposort) {
  //      val pred = graph.predecessorsOf(e)
  //
  //      if ((!pred.isEmpty) && pred.forall(deleted contains) && !deleted.contains(e) && !dontDelete.contains(e)) {
  //        deleted = e :: deleted
  //      }
  //    }
  //  }

  def cleanNodes(graph: Graph, kept: List[Cluster], clan: List[EventGroup] = Nil): Graph =
    {
      var removedNodes = List[Cluster]()
      val realClans =
        if (clan == Nil) {
          UnitAnalysis.findClans(graph)
        } else {
          clan
        }

      //      println("rc " + realClans)

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
        val cleanedGraph = graph.detectAndAddSkipLinks(removedNodes).removeNodes(removedNodes)
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
      var oneMoreLoop = false

      // remove as many nodes as we can
      var cleanGraph = cleanNodesNew(graph, keptNodes)

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
              //              println("kept more " + clan.nodes)
              oneMoreLoop = true
            }
          }
        }

        if (oneMoreLoop) {
          keptNodes = newKept.distinct
          cleanGraph = cleanNodesNew(cleanGraph, keptNodes)
          i += 1
          cleanGraph.draw("clean" + i)
        }
      }

      // remove the START and the END, and put them back so they are behave properly 
      val start = cleanGraph.nodes.find(_.name == "START").get
      val end = cleanGraph.nodes.find(_.name == "END").getOrElse(null)

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

      //      println("delete starting with " + deletedNodes.mkString)
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