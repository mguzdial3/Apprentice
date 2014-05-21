package edu.gatech.eilab.scheherazade.graph.structure

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
object MutexAnalysisBad {

  def main(args: Array[String]) {
    val before = SampleGraph.sample16
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(3)
    graph.draw("special-test")
    val kept = List(background)
    println(kept)
    //    val r = causeForDeletionNew(graph, kept)
    //    println("causes =" + r._1.mkString("\n"))
    cleanNodes3(graph, kept)
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

  def transClosureWithDenial(graph: Graph, kept: List[Cluster], dont: List[Cluster]) =
    {
      var deleted = List[Cluster]()

      // immediately excluded
      for (me <- graph.mutualExcls) {
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
  def cleanNodes3(graph: Graph, kept: List[Cluster]) = {

    var dontDelete = List[Cluster]()
    var deleted = List[Cluster]()
    val causesMap = HashMap[Cluster, List[Set[Cluster]]]() // the cause for deleting a node

    // first, events immediately excluded by events in the kept list
    for (me <- graph.mutualExcls) {
      println(me)
      if (kept.contains(me.c1)) {
        deleted = me.c2 :: deleted
        addCause(causesMap, me.c1, me.c2) // me.c2 is the cause
      } else if (kept.contains(me.c2)) {
        deleted = me.c1 :: deleted
        addCause(causesMap, me.c2, me.c1) // me.c1 is the cause
      } else {
        addCause(causesMap, me.c1, me.c2)
        addCause(causesMap, me.c2, me.c1) // either can cause the other to be excluded
      }
      println(causesMap.mkString("\n"))
      println("***")

    }
    //    println("immediately deleted = " + deleted)
    deleted = deleted.distinct

    /*** second, delete nodes by transitive closure ***/
    val topoOrder = graph.topoSort

    for (e <- topoOrder) {
      val pred = graph.predecessorsOf(e)
      if ((!pred.isEmpty) && pred.forall(deleted contains) && !deleted.contains(e)) {
        deleted = e :: deleted
      }
    }
    //    println("transtively deleted = " + deleted)

    println("all causes " + causesMap.mkString("\n"))
    /*** Third, propogate the causes through the graph from parents to children ***/
    val insertedLinks = HashMap[Cluster, List[Cluster]]()
    for (e <- topoOrder) {
      //      println("processing : " + e)
      var furtherProp = true // whether we want to propagate further
      var resultsToSave = List[Set[Cluster]]()

      val pred = graph.predecessorsOf(e)

      println("causes contains " + e.name + " " + causesMap.contains(e))
      // only if all predecessors can be deleted; added to handle Sample Graph 18
      if (pred.forall(prd => causesMap.contains(prd)) || causesMap.contains(e)) {

        // AND relation between all predecessors
        var allCauses =
          causesMap.filter(p => pred.contains(p._1)).map(_._2) // retrieve all cause lists

        println("all causes " + allCauses.mkString("\n"))
        allCauses = allCauses.map(listOfSets => listOfSets.filterNot(set => set.exists(n => graph.shortestDistance(n, e) != -1)).filterNot(_ == Nil))
        // NOTE: the reason for this filtering is that a cause to delete only matters 
        // if the cause is parallel to the event or ordered after the event
        // the reason for using "exist" rather than "forall" is that events in one set are "ANDed" together
        // thus, all needs to be parallel or after the event e.

        println("all causes " + allCauses.mkString("\n"))
        var comb = List[Set[Cluster]]()
        if (allCauses != Nil) { // combine the causes from predecessors
          comb = allCauses.head
          allCauses = allCauses.tail
          while (allCauses != Nil) {
            val next = allCauses.head
            allCauses = allCauses.tail
            comb = comb.flatMap(x => next.map(y => x ++ y)).filterNot(s => s.size == 0)
          }
        }

        // combine with existing causes
        // now comb is a list of sets. The sets are OR-ed together
        comb = (causesMap.getOrElse(e, Nil) ::: comb).distinct
        resultsToSave = comb

        println("comb " + comb)
        if (e.name == "C4") {
          println("")
        }

        // we first find causes that are totally parallel to the same child of e.
        val kids = graph.links.filter(l => l.source == e).map(_.target)
        val kidsRemain = kids.filterNot(deleted.contains)
        println("kids remain" + kidsRemain.mkString(", "))
        val parallelCauses = comb.filter(set => kidsRemain.exists(k => set.forall(x => !graph.ordered(x, k))))

        val pCauses = // causes that are also parallel to e
          parallelCauses.filter { set =>
            set.forall(s => !graph.ordered(e, s))
          }
        val count = pCauses.size
        println("count = " + count)
        if (count >= 2) {
          // e is no-delete
          dontDelete = e :: dontDelete
          furtherProp = false
        } else if (count == 1) {
          // if e is optional,  only needs 1
          if (graph.optionals.contains(e) &&
            parallelCauses.filterNot(pCauses.contains).exists(set => // another cause
              set.forall(s => graph.shortestDistance(e, s) != -1 || graph.shortestDistance(s, e) == -1))) {
            // e is no-delete
            dontDelete = e :: dontDelete
            furtherProp = false
          } else if (!graph.isOptional(e)) {

            // e is not no-delete, but do we need any links inserted?
            val cause = pCauses.head
            //testInsertedLink(cause, e, kidsRemain, graph, insertedLinks)

            val pass = cause.forall(c => !graph.ordered(c, e)) ||
              (graph.isOptional(e) &&
                cause.forall(s => graph.shortestDistance(e, s) != -1 || graph.shortestDistance(s, e) == -1))

            if (pass) {
              println("spec " + e.name + causesMap.getOrElse(e, Nil))
              if (causesMap.getOrElse(e, Nil).exists(set => set.forall(s => cause.forall(c => !graph.ordered(c, s)))
                )) // another node can also delete e
                {
                dontDelete = e :: dontDelete
                furtherProp = false
              } else {
                val linkKids = kidsRemain.filter(k => cause.forall(c => !graph.ordered(c, k)))
                for (k <- linkKids) {
                  val old = insertedLinks.getOrElse(k, Nil)
                  insertedLinks += (k -> (old ::: cause.toList))
                }
              }
            }

          }
        } else if (graph.isOptional(e)) {
          // if e is optional and causes are later than e and parallel to e's kids 
          val cCauses = parallelCauses.filter(set =>
            set.forall(s => graph.shortestDistance(e, s) != -1 || graph.shortestDistance(s, e) == -1))

          val cCount = cCauses.size
          if (cCount >= 2) {
            dontDelete = e :: dontDelete
            furtherProp = false
          } else if (cCount == 1) {
            // e is not no-delete, test for inserted links
            //            val head = cCauses.head
            //            testInsertedLink(head, e, kidsRemain, graph, insertedLinks)
          }
        }

        // not no-delete, no inserted links, do nothing besides further propogation

      } else { // no reason to delete e, no need to propagate further
        furtherProp = false
      }

      if (furtherProp) {
        if (resultsToSave != Nil) {
          causesMap += (e -> resultsToSave)
        } else {
          causesMap.remove(e)
        }
      } else {
        causesMap.remove(e)
      }

    }

    println("need to insert link " + insertedLinks)
    println("dont delete " + dontDelete)
    println("should delete: " + deleted)

    deleted = transClosureWithDenial(graph, kept, dontDelete)
    println("actually delete: " + deleted)

    val newLinks = insertedLinks.flatMap {
      case (n, list) =>
        list.map(x => new Link(x, n))
    }.toList

    reformGraph(graph, deleted, newLinks)

  }

  def reformGraph(graph: Graph, deleted: List[Cluster], newLinks: List[Link]): Graph =
    {
      if (deleted != Nil) {
        val newGraph = graph.detectAndAddSkipLinks(deleted).removeNodes(deleted)
        //val newGraph = graph.addSkipLinks(removedNodes).removeNodes(removedNodes)
        val nl = newLinks.filter(l => newGraph.nodes.contains(l.source) && newGraph.nodes.contains(l.target))
        // we should NOT re-check optionality because of deleted events! we must keep the original optional events!
        new Graph(newGraph.nodes, nl ::: newGraph.links, newGraph.mutualExcls, newGraph.optionals, newGraph.conditionals)
      } else {
        graph
      }
    }

  private def testInsertedLink(cause: Set[Cluster], curNode: Cluster, kidsRemain: List[Cluster], graph: Graph, insertedLinks: HashMap[Cluster, List[Cluster]]) {
    // (cause || e) OR (e is optional AND (e -> causes OR e || causes))  
    val pass = cause.forall(c => !graph.ordered(c, curNode)) ||
      (graph.isOptional(curNode) &&
        cause.forall(s => graph.shortestDistance(curNode, s) != -1 || graph.shortestDistance(s, curNode) == -1))

    if (pass) {
      val linkKids = kidsRemain.filter(k => cause.forall(c => !graph.ordered(c, k)))
      for (k <- linkKids) {
        val old = insertedLinks.getOrElse(k, Nil)
        insertedLinks += (k -> (old ::: cause.toList))
      }
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
      var cleanGraph = cleanNodes3(graph, keptNodes)

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

  //TODO: If node A is kept, and node A has only one (non-optional) parent, the parent must be kept.
  // If A has two parents, who always happen together, they must both happen as well. Thus, we need detection of always-happen-togethers
}