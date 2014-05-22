package edu.gatech.eilab.scheherazade.graph.structure

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
object MutexAnalysis {

  def main(args: Array[String]) {
    val graph = SampleGraph.sample18.graphWithOptionalsAndSkips
    graph.draw("special-test")
    val kept = List(graph.nodes(5))
    println(kept)
    val r = causeForDeletionNew(graph, kept)
    println("causes =" + r._1.mkString("\n"))
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
      var remainder = graph.nodes filterNot (all.contains)
      do {
        newFound = ListBuffer[Cluster]()
        for (e <- remainder) {
          val pred = graph.predecessorsOf(e)
          if ((!pred.isEmpty) &&
            pred.forall(all.contains))
            newFound += e
        }
        all ++= newFound
        remainder = remainder filterNot (newFound.contains)
      } while (!newFound.isEmpty)

      all.toList
    }

  def transClosureWithDenial(graph: Graph, kept: List[Cluster], dont: List[Cluster]) =
    {
      var deleted = List[Cluster]()

      // immediately excluded
      for (me <- graph.mutualExcls) {
        var excluded: Cluster = null
        var cause: Cluster = null
        if (kept.contains(me.c1)) {
          deleted = me.c2 :: deleted
        } else if (kept.contains(me.c2)) {
          deleted = me.c1 :: deleted
        }
      }

      deleted = deleted.filterNot(dont.contains) // dont prevents a cluster from being deleted

      val toposort = graph.topoSort

      for (e <- toposort) {
        val pred = graph.predecessorsOf(e)
        if ((!pred.isEmpty) && pred.forall(deleted contains)) {

          if ((!deleted.contains(e)) && (!dont.contains(e))) { // dont prevents a cluster from being deleted
            deleted = e :: deleted
          }
        }
      }
      deleted
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
    var recursiveDeleted = List[Cluster]()
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

    //    println("immediately deleted = " + deleted)

    // delete nodes by transitive closure
    val toposort = graph.topoSort
    for (e <- toposort) {
      val pred = graph.predecessorsOf(e)

      if ((!pred.isEmpty) && pred.forall(deleted contains) && !deleted.contains(e)) {
        deleted = e :: deleted
        recursiveDeleted = e :: recursiveDeleted
      }
    }

    deleted = deleted.distinct

    //    println("transtively deleted = " + deleted)

    //val possibleCauses = graph.nodes.filterNot(deleted.contains)

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

    //    println("all removed and their causes " + causes)
    // Third, propogate the causes through the graph

    for (e <- toposort) {
      //      println("processing : " + e)
      // needs to get causes from parents.
      val pred = graph.predecessorsOf(e)

      // if all predecessors can be deleted

      if (pred.forall(prd => causes.contains(prd))) { // this is added to handle sample graph 18

        // AND relation between all predecessors
        var allCauses = causes.filter(p => pred.contains(p._1)).map(_._2) // retrieve all cause lists
        //        println("all causes 1 " + allCauses.map(_.mkString(" OR ")).mkString(", "))
        allCauses = allCauses.map {
          listOfSets =>
            listOfSets.filterNot {
              set =>
                set.exists(
                  x =>
                    graph.shortestDistance(x, e) != -1 && (!graph.isOptional(x)))
            }
        }.filterNot(_ == Nil)

        //        println("all causes 2 " + allCauses.map(_.mkString(" OR ")).mkString(", "))

        if (allCauses != Nil) { // combine the causes from predecessors
          var comb = allCauses.head
          allCauses = allCauses.tail

          while (allCauses != Nil) {
            val next = allCauses.head //.filterNot(set => set.exists(n => graph.shortestDistance(n, e) != -1))
            allCauses = allCauses.tail

            comb = comb.flatMap(x => next.map(y => x ++ y)).filterNot(s => s.size == 0)
            //            println("comb " + comb)
          }

          if (causes.contains(e)) {
            val old = causes(e)
            val newl = (comb ::: old).distinct
            if (newl != Nil) {
              causes += (e -> newl) // OR relation
            } else {
              causes.remove(e)
            }
          } else if (comb != Nil) {
            causes += (e -> comb)
          }
        }
      }

    }

    (causes, deleted, recursiveDeleted)
  }

  /**
   * finds who is responsible for deleting what
   *
   */
  def causeForDeletionNewDenial(graph: Graph, kept: List[Cluster], dontDelete: List[Cluster]) = {

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

    deleted = deleted.filterNot(dontDelete.contains)

    //    println("immediately deleted = " + deleted)

    // delete nodes by transitive closure
    val toposort = graph.topoSort
    for (e <- toposort) {
      val pred = graph.predecessorsOf(e)

      if ((!pred.isEmpty) && pred.forall(deleted contains) && !deleted.contains(e)) {
        deleted = e :: deleted
      }
    }

    deleted = deleted.distinct

    //    println("transtively deleted = " + deleted)

    //val possibleCauses = graph.nodes.filterNot(deleted.contains)

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

    //    println("all removed and their causes " + causes)
    // Third, propogate the causes through the graph

    for (e <- toposort) {
      //      println("processing : " + e)
      // needs to get causes from parents.
      val pred = graph.predecessorsOf(e)

      // only if all predecessors can be deleted, should we pass on deletion causes, this is added to handle sample graph 18
      // only if the node may be deleted, this is for another special case.
      if (pred.forall(prd => causes.contains(prd)) && !dontDelete.contains(e)) { // 

        // AND relation between all predecessors
        var allCauses = causes.filter(p => pred.contains(p._1)).map(_._2) // retrieve all cause lists
        //        println("all causes 1 " + allCauses.map(_.mkString(" OR ")).mkString(", "))
        // each item is a list. For each set in any list, remove it if it contains events that are not predecessors of e 
        // now I don't understand why
        //allCauses = allCauses.map(listOfSets => listOfSets.filterNot(set => set.exists(n => graph.shortestDistance(n, e) != -1)))

        //        println("all causes 2 " + allCauses.map(_.mkString(" OR ")).mkString(", "))

        if (allCauses != Nil) { // combine the causes from predecessors
          var comb = allCauses.head
          allCauses = allCauses.tail

          while (allCauses != Nil) {
            val next = allCauses.head //.filterNot(set => set.exists(n => graph.shortestDistance(n, e) != -1))
            allCauses = allCauses.tail

            comb = comb.flatMap(x => next.map(y => x ++ y)).filterNot(s => s.size == 0)
            //            println("comb " + comb)
          }

          if (causes.contains(e)) {
            val old = causes(e)
            val newl = (comb ::: old).distinct
            if (newl != Nil) {
              causes += (e -> newl) // OR relation
            } else {
              causes.remove(e)
            }
          } else if (comb != Nil) {
            causes += (e -> comb)
          }
        }
      }

    }

    (causes, deleted)
  }

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

  def threeOrMoreRancingConditions(causeN: List[Set[Cluster]]): Boolean =
    {
      var remainCauses =
        for (set <- causeN) yield {
          val rest = causeN.filterNot(_ == set)
          var ns = set
          for (rs <- rest) {
            if (rs.forall(k => ns.contains(k))) {
              ns = ns -- rs
            }
          }
          ns
        }

      remainCauses = remainCauses.filterNot(_.isEmpty)

      remainCauses.size >= 3
    }

  /* a new version of clean nodes that fix the special case
   * 
   */
  def cleanNodesNew(graph: Graph, kept: List[Cluster]): Graph =
    {
      val (causes, d, recursiveDeleted) = causeForDeletionNew(graph, kept)
      // recursiveDeleted contains nodes deleted because their parents are all deleted, not because they are deleted by mutual exclusions directly 
      var removedNodes = d
      //      println("causes = " + causes)
      //      println("d = " + removedNodes)
      /* Special care is needed when node C excludes node A AND
	   * node A is a predecessor to node B AND 
	   * B is parallel to C and not marked for deletion 
	  */
      val insertLink = HashMap[Cluster, List[Cluster]]()
      var dontDelete = List[Cluster]()
      for (n <- removedNodes if (!graph.optionals.contains(n))) { //&& (! recursiveDeleted.contains(n))) {
        // don't perform this test if n is optional
        // because an optional event is not a precondition for anything, 
        // so removing it is not removing any preconditions

        val potential = HashMap[Cluster, List[Cluster]]()

        val kids = graph.successorsOf(n)
        val causeN = causes(n)

        println("considering " + n.name)
        println("causeN " + causeN)

        // ignore graphs with three or more events racing against each other
        val ignore = threeOrMoreRancingConditions(causeN)
        if (ignore) {
          return graph
        }

        val parallelCauses = causeN.map { AndedCause =>
          AndedCause.filter { c =>
            var exists = false
            for (k <- (kids.filterNot(removedNodes.contains))) {
              if (!graph.ordered(c, k)) {
                println("unordered: " + k.name + " with cause " + c.name)
                exists = true
                potential += (k -> (c :: potential.getOrElse(k, Nil)))
              }
            }
            exists
          }
        }.filterNot(_.isEmpty)

        println("parallel causes = " + parallelCauses)
        if (parallelCauses.size == 1) {

          val pCauses = parallelCauses.head
          val bool = causeN.filterNot(_ == pCauses).exists { set =>
            set.forall(s => graph.isOptional(s) || (!graph.ordered(s, n)))
          } // there may be another causer that is optional or parallel to node n

          /*val bool = pCauses.forall(pc => graph.nodes.exists(node =>
            node != n &&
              node != pc &&
              graph.shortestDistance(node, pc) != -1 &&
              //            (!graph.ordered(node, n)) &&
              graph.mutuallyExclusive(node, n)))
              * 
              */
          // exists a node that is parallel to the cause of deletion (pc), the node is also mutual exclusive to n.
          if (bool) {
            println("add don't delete 1 " + n)
            dontDelete = n :: dontDelete
          } else {
            // end of tentative
            println("must insert link " + potential)
            insertLink ++= potential
          }
        } else if (parallelCauses.size > 1) {
          println("add don't delete 2 " + n)
          dontDelete = n :: dontDelete
        }
      }
      println("should delete: " + removedNodes)
      dontDelete = dontDelete.filterNot(recursiveDeleted.contains)

      /** another possibility for race condition: there are two parallel deletion causes, who lead to the deletion of different children of n **/
      val dontFromTransClosure = detectRaceConditionForTransClosure(graph, kept, causes, removedNodes)
      if (!dontFromTransClosure.isEmpty) {
        return graph
      }
      //      dontDelete = dontFromTransClosure ::: dontDelete

      removedNodes = transClosureWithDenial(graph, kept, dontDelete)
      println("actually delete: " + removedNodes)
      //removedNodes = removedNodes.filterNot(dontDelete.contains) // try the above line
      insertLink --= removedNodes
      println("need to insert link " + insertLink)
      println("dont delete " + dontDelete)
      println("delete = " + removedNodes)

      if (removedNodes != Nil) {
        val newGraph = graph.detectAndAddSkipLinks(removedNodes).removeNodes(removedNodes)
        //val newGraph = graph.addSkipLinks(removedNodes).removeNodes(removedNodes)
        // we should NOT re-check optionality because of deleted events! we must keep the original optional events!
        val newLinks = insertLink.flatMap {
          case (n, list) =>
            list.map { x => new Link(x, n, "T") // these are "T" links which should not used to determine if a node can be deleted or not.
            }
        }.toList

        new Graph(newGraph.nodes, newLinks ::: newGraph.links, newGraph.mutualExcls, newGraph.optionals, newGraph.conditionals)
      } else {
        graph
      }
    }

  def detectRaceConditionForTransClosure(graph: Graph, kept: List[Cluster], causes: HashMap[Cluster, List[Set[Cluster]]], removed: List[Cluster]): List[Cluster] =
    {
      var dontDelete = List[Cluster]()

      for (n <- removed if (!graph.optionals.contains(n)) && causes(n).size > 1) {
        val causeForN = causes(n)

        val extra = kept.filterNot(k => causeForN.exists(set => set.contains(k)))

        val recursive = causeForN.map {
          set =>
            findTransitiveClosure(graph, extra ::: set.toList).toSet
        }

        val identical = recursive.sliding(2).forall(list => list(0) == list(1))
        if (!identical) {
          dontDelete = n :: dontDelete
          println("don't delete from different trans closure: " + n)
        }
      }

      dontDelete
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

  //  def cleanNodes(graph: Graph, kept: List[Cluster], clan: List[EventGroup] = Nil): Graph =
  //    {
  //      var removedNodes = List[Cluster]()
  //      val realClans =
  //        if (clan == Nil) {
  //          UnitAnalysis.findClans(graph)
  //        } else {
  //          clan
  //        }
  //
  //      //      println("rc " + realClans)
  //
  //      for (me <- graph.mutualExcls) {
  //        if (kept.contains(me.c1) && !kept.contains(me.c2)) {
  //          removedNodes = me.c2 :: removedNodes
  //        } else if (kept.contains(me.c2) && !kept.contains(me.c1)) {
  //          removedNodes = me.c1 :: removedNodes
  //        }
  //      }
  //      //println("removed: " + removedNodes.map(_.name).mkString)
  //
  //      for (c <- realClans) {
  //        val clan = c.nodes
  //        if (clan.exists(removedNodes.contains)) {
  //          removedNodes = clan ::: removedNodes
  //        }
  //      }
  //
  //      removedNodes = findTransitiveClosure(graph, removedNodes)
  //
  //      removedNodes = removedNodes.distinct
  //      //println("removed: " + removedNodes.map(_.name).mkString)
  //
  //      if (removedNodes != Nil) {
  //        val cleanedGraph = graph.detectAndAddSkipLinks(removedNodes).removeNodes(removedNodes)
  //        //val cleanedGraph = graph.addSkipLinks(removedNodes).removeNodes(removedNodes)
  //        // we should NOT re-check optionality because of deleted events! we must keep the original optional events!
  //
  //        cleanedGraph
  //      } else {
  //        graph
  //      }
  //    }

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

      //cleanGraph.draw("clean-graph")

      // remove the START and the END, and put them back so they are behave properly 
      val start = cleanGraph.nodes.find(_.name == "START").get
      val end = cleanGraph.nodes.find(_.name == "END").getOrElse(null)

      val g1 = new Graph(cleanGraph.nodes.filterNot(v => v == start || v == end), // remove start and ends
        cleanGraph.links.filterNot(l => l.source == start || l.target == end),
        cleanGraph.mutualExcls,
        cleanGraph.optionals, // optional events have to remain optional
        cleanGraph.conditionals.filterNot(n => !cleanGraph.mutualExcls.exists(me => me.c1 == n || me.c2 == n)) // conditionals become normal if the corresponding optional event has been removed
        )

      val g2 = AnalysisMain.addStartEnd(g1)

      // SPECIAL CASE: if an event is linked to the start event in the original graph, 
      // it should still be linked unless it has been removed
      val originalStartLinks = graph.links.filter(_.source.name == "START").filter(l => g2.nodes.contains(l.target))
      val g3 = new Graph(g2.nodes, (g2.links ::: originalStartLinks).distinct, g2.mutualExcls,
        g2.optionals, g2.conditionals)

      g3
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