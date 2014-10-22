package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object CfRComputer {

  var group2cfr: HashMap[Cluster, List[List[Cluster]]] = null

  /**
   * must call this init function before computing any CfR for any graphs
   *
   */
  def init() {
    group2cfr = HashMap[Cluster, List[List[Cluster]]]()
  }

  /**
   * computes the vertices that are directly invovled in mutual exclusion relations
   *
   */
  def immediateMutex(graph: Graph): HashMap[Cluster, List[List[Cluster]]] =
    {
      val map = HashMap[Cluster, List[List[Cluster]]]()
      for (mx <- graph.mutualExcls) {
        val c1 = mx.c1
        val c2 = mx.c2

        var c1List = map.getOrElse(c1, List[List[Cluster]]())
        c1List = List(c2) :: c1List
        map.update(c1, c1List)

        var c2List = map.getOrElse(c2, List[List[Cluster]]())
        c2List = List(c1) :: c2List
        map.update(c2, c2List)
      }

      map
    }

  /**
   * Propagates causes for removal from parents to children.
   * Note: This function modifies the content of the map object.
   *
   */
  def propFromParent(graph: Graph, map: HashMap[Cluster, List[List[Cluster]]], order: List[Cluster]): (HashMap[Cluster, List[List[Cluster]]], List[RaceCondition]) =
    {

      var raceConditions = List[RaceCondition]()
      val potentialCfR = HashMap[Cluster, List[List[Cluster]]]()
      for (c <- order if graph.parentsOf(c) != Nil) {
        println("propagating for vertex " + c.name)
        val parents = graph.parentsOf(c)

        val allPossibleAssignment = assignParents(c, parents, graph, map)
        //println(c)
        println(allPossibleAssignment)
        var list = map.getOrElse(c, List[List[Cluster]]())
        var newCfR = List[List[Cluster]]()
        for (assignment <- allPossibleAssignment) {
          // filtering bad solutions and collect correct solutions
          val answer = collectFeasible(assignment, graph, map)
          //println("answer =" + answer)

          val cfrAnswer = answer._1.filterNot(list => list.contains(c)) // a CfR cannot include the event itself
          if (answer._1 != Nil) {
            newCfR = cfrAnswer ::: newCfR
          }
          if (answer._2 != Nil) {
            raceConditions = answer._2 ::: raceConditions

            val potentialList = answer._2.map(i => i.a ::: i.b)
            println("potentialList = " + potentialList) //TODO: finish the recognition of potential lists
            potentialCfR.update(c, potentialList)
          }
        }

        val cfrList = simplify((newCfR ::: list).distinct)
        map.update(c, cfrList)
      }

      (map, raceConditions)

    }

  def assignParents(c: Cluster, parents: List[Cluster], graph: Graph, map: HashMap[Cluster, List[List[Cluster]]]): List[(ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster])] =
    {
	  println(parents)
      val tuple = (ListBuffer[Cluster](), ListBuffer[Cluster](), ListBuffer[Cluster]())
      var curList = ListBuffer[(ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster])]()
      curList += tuple

      for (p <- parents) {
    	  
        /**temp code starts**/
        if (c.name == "j") {
          println("doing parent " + p.name)
        }
        /**temp code ends**/

        if (isCaseOne(p, graph, map)) {
          for (tuple <- curList) {
            tuple._1 += p
          }
        } else {
          val caseTwo = isCaseTwo(p, graph, map)
          val caseThree = isCaseThree(p, graph, map)

          if (!caseTwo && !caseThree) {
            // impossible assignment: return the equivalent of Nil
            return Nil
          } else if (caseTwo && !caseThree) {
            for (tuple <- curList) {
              tuple._2 += p
            }
          } else if (!caseTwo && caseThree) {
            for (tuple <- curList) {
              tuple._3 += p
            }
          } else {
            // both case two and case three. This is a difficult case
            val curListCopy = curList.clone
            for (tuple <- curListCopy) {
              val clone = (ListBuffer[Cluster]() ++ tuple._1, ListBuffer[Cluster]() ++ tuple._2, ListBuffer[Cluster]() ++ tuple._3)
              tuple._2 += p
              clone._3 += p
              curList += clone
            }
          }
        }
      }

      curList.toList
    }

  def isCaseOne(p: Cluster, graph: Graph, hashmap: HashMap[Cluster, List[List[Cluster]]]): Boolean =
    {
      graph.parentsOf(p) == Nil && hashmap.contains(p) && hashmap(p) != Nil
    }

  def isCaseTwo(p: Cluster, graph: Graph, map: HashMap[Cluster, List[List[Cluster]]]): Boolean =
    {
      if (!map.contains(p)) return false // no cfr for p

      val cause4p = map(p)
      var grandparents = graph.parentsOf(p)

      if (grandparents == Nil) {
        return false
      }

      var visited = List[Cluster]()
      var queue = Queue[Cluster]()
      queue.enqueue(grandparents: _*)

      var intersection: List[List[Cluster]] = cause4p
      while (!queue.isEmpty) {
        val gp = queue.dequeue

        if (!map.contains(gp)) {
          return false
        }
        val cause4gp = map(gp)
        // if there does not exist a cfr for p that is a superset of one cfr for gp, return false
        intersection = intersection.filter(c4p => cause4gp.exists(c4gp => c4gp.forall(c4p.contains)))
        if (intersection == Nil) {
          return false
        }

        visited = gp :: visited
        val pred = graph.parentsOf(gp).filter(visited.contains)
        queue.enqueue(pred: _*)

      }

      //println("update " + p + " with  " + intersection)
      group2cfr.update(p, intersection)

      return true
    }

  def isCaseThree(c: Cluster, graph: Graph, map: HashMap[Cluster, List[List[Cluster]]]): Boolean =
    {
      graph.parentsOf(c) != Nil && map.contains(c) && map(c).exists(_.size == 1)
    }

  /** collects the CfR from a possible parent assignment
   *  returns two things: a list of CfRs, and a list of race conditions
   *     
   */
  def collectFeasible(grouping: (ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster]), graph: Graph, hashmap: HashMap[Cluster, List[List[Cluster]]]): (List[List[Cluster]], List[RaceCondition]) =
    {

      // check for empty list
      if (grouping._1.size == 0 && grouping._2.size == 0 && grouping._3.size == 0) {
        return (Nil, Nil)
      }

      var results = List[List[Cluster]]()

      // test for group three
      val g3 = grouping._3
      if (g3 != Nil) {
        val c4g3 = g3.map(hashmap(_).filter(_.size == 1)) // cfr with only one node
        //println("cfrs in group 3. Do you see one cfr shared by all vertices? " + c4g3)
        if (c4g3 != Nil) { // checking if there is one CfR that is shared by all vertices
          var intersection = c4g3.head
          for (n <- c4g3.tail) {
            intersection = intersection.intersect(n)
            if (intersection == Nil) return (Nil, Nil)
          }
          results = intersection
        } else {
          return (Nil, Nil) // nothing with size == 1
        }
      }

      // collect cfr from group 2
      val g2 = grouping._2
      var group2Collection: List[List[Cluster]] = Nil
      for (p <- g2) {
        val cfr = group2cfr(p) // need to compute a set product
        //println("cfr for " + p + "is " + cfr)
        if (group2Collection == Nil) {
          group2Collection = cfr
        } else {
          group2Collection = setProduct(group2Collection, cfr)
        }
      }
      group2Collection = group2Collection.distinct

      // collect group 1
      val g1 = grouping._1
      var g1result: List[List[Cluster]] = Nil

      if (g1 != Nil) {
        g1result = hashmap(g1.head)
        for (n <- g1.tail) {
          val r = hashmap(n)
          g1result = setProduct(g1result, r)
        }
      }

      // this is a set product
      if (g1result != Nil && group2Collection != Nil) {
        group2Collection = setProduct(group2Collection, g1result)
      } else if (g1result != Nil && group2Collection == Nil) {
        group2Collection = g1result
      }

      if (results == Nil && group2Collection != Nil) {
        return (group2Collection, Nil)
      } else if (results != Nil && group2Collection == Nil) {
        return (results, Nil)
      } else {
        // cfr in group three must happen after every nodes in group two and one
        val pairs = for (x <- results; y <- group2Collection) yield (x, y)

        var good = List[List[Cluster]]()
        var raceConditions = List[RaceCondition]()
        var potentialCfRs = List[List[Cluster]]()
        for (p <- pairs) {
          val after = p._1.head
          val before = p._2
          val validCfR = before.forall(x => graph.shortestDistance(x, after) > 0) // $after$ is ordered after every other vertex. This is a valid CfR
          val impossibleCfR = before.exists(x => graph.shortestDistance(after, x) > 0) 
          // $after$ is ordered before every other vertex. no orderings would make it a CfR. Thus, this is impossible.
          
          //println("check ordering: " + passTest)
          if (validCfR) {
            good = (p._1 ::: p._2) :: good
          } else if (!impossibleCfR) {            
            // if not impossible, then some orderings can delete the vertex, while other orderings will keep the vertex. Therefore it is a race condition.
            raceConditions = new RaceCondition(p._1, p._2) :: raceConditions
          }
          else
          {
            //println(" Haha! impossible CfR detected: " + after.name + ", " + before.map(_.name).mkString(";")) 
          }
        }

        return (good, raceConditions)
      }
    }

  def setProduct(set1: List[List[Cluster]], set2: List[List[Cluster]]) =
    {
      for (x <- set1; y <- set2) yield (x ::: y).distinct
    }

  def simplify(cfrList: List[List[Cluster]]): List[List[Cluster]] =
    {
      var list = List[List[Cluster]]()
      for (cfr <- cfrList) {
        if (!cfrList.exists(
          x => x.forall(cfr.contains) && (!cfr.forall(x.contains)))) {
          list = cfr :: list
        }
      }

      list
    }

  def main(args: Array[String]) {
    init()
    val graph = CfRSample.graph7().graphWithOptionalsAndSkips
    graph.draw("aaaa")
    var map = immediateMutex(graph)
    //println(formatMap(map))
    val order = graph.topoSort
    println("topological sort = " + order.map(_.name).mkString)
    val answer = propFromParent(graph, map, order)
    map = answer._1
    val raceConditions = answer._2
    //println(formatMap(map))
    //println(raceConditions)
    //    testSimplification()
  }

  def testSimplification() {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)

    val l1 = List(a, b, c)
    val l2 = List(a, c)
    val l3 = List(b, c)

    val list = List(l1, l2, l3)
    //println(simplify(list))
  }

  def processGraph(graph: Graph): (HashMap[Cluster, List[List[Cluster]]], List[RaceCondition]) =
    {
      init()
      var map = immediateMutex(graph.graphWithOptionalsAndSkips)
      val order = graph.topoSort
      println("topological sort = " + order.map(_.name).mkString)
      val answer = propFromParent(graph, map, order)

      val raceCond = RaceConditionsType101(graph, order, answer._1)
      (answer._1, (raceCond ::: answer._2).distinct)
    }

  def formatMap(map: HashMap[Cluster, List[List[Cluster]]]): String =
    {
      val str = new StringBuffer()
      for (key <- map.keySet) {
        str.append(key.name)
        str.append(" -> { ")
        for (values <- map(key)) {
          str.append(values.map(_.name).mkString("{", ",", "}"))
        }
        str.append(" }, ")
      }

      str.toString
    }

  /**
   * detect Type-I (1) race conditions
   * Attention: modifies the hashmap
   */
  def RaceConditionsType101(graph: Graph, order: List[Cluster], cfr: HashMap[Cluster, List[List[Cluster]]]): List[RaceCondition] = {
    var raceConditions = List[RaceCondition]()

    for (c <- order if cfr.contains(c) && cfr(c) != Nil) // only need to check existing cfr
    {
      val directMutex = cfr(c).filter(x => x.size == 1 && graph.mutuallyExclusive(x.head, c))
      val childCfR = cfr(c).filterNot(directMutex.contains)

      println("prcessing " + c.name)
      var removed = List[List[Cluster]]()

      for (p <- graph.parentsOf(c) if cfr.contains(p) && cfr(p) != Nil) {
        var parentsCfR = cfr(p)
        parentsCfR = parentsCfR.filter { pcfr =>
          !childCfR.exists(ccfr => pcfr.forall(ccfr.contains))
        }

        for (ccfr <- childCfR; pcfr <- parentsCfR) {
          if (preventsCfR(graph, pcfr, ccfr)) {
            removed = ccfr :: removed
            println("newly removed: " + ccfr)
          } else if (parallelCfR(graph, pcfr, ccfr)) {
            raceConditions = new RaceCondition(pcfr, ccfr) :: raceConditions
            println("new race discovered: " + pcfr.mkString("(", ",", ")") + ccfr.mkString("(", ",", ")"))
          }
        }

      }

      cfr.update(c, childCfR.filterNot(removed.contains) ::: directMutex)
    }
    println("101 race conditoins = " + raceConditions)
    raceConditions
  }

  /**
   * Tests if one list of clusters A totally precedes the other list of clusters B so that A prevents B from working as a CfR.
   *  First, we only consider the non-overlapping parts of both lists. If the two lists are identical, we return false.
   *  After that, if every vertex in the remaining part of list A is ordered before the remaining part of list B,
   *  we return true. Otherwise, return false
   */
  def preventsCfR(graph: Graph, A: List[Cluster], B: List[Cluster]): Boolean =
    {
      val remainA = A.filterNot(B.contains)
      val remainB = B.filterNot(A.contains)
      val common = A.filter(B.contains)
      if (remainA == Nil || remainB == Nil) {
        // if remainA is Nil, A is part of the CfR for B.
        // if remainB is Nil, A cannot happen without B happen, which means the node will be removed. So B is a real CfR.
        return false
      } else {
        // neither remainA or remainB is Nil
        // each element of A precedes all elements of B and is not optional. 
        // If it is optional, it may not execute, which allows B to work as a CfR
        remainA.forall(x => remainB.forall(y => graph.shortestDistance(x, y) != -1) && !graph.isOptional(x))
      }
    }

  /**
   * Tests if one list of clusters A is parallel to the other list of clusters B so that A and B become a race condition.
   *  First, we only consider the non-overlapping parts of both lists. If the two lists are identical, we return false.
   *  After that, if every vertex in the remaining part of list A is ordered before the remaining part of list B,
   *  we return true. Otherwise, return false
   */
  def parallelCfR(graph: Graph, A: List[Cluster], B: List[Cluster]): Boolean =
    {
      val remainA = A.filterNot(B.contains)
      val remainB = B.filterNot(A.contains)
      val common = A.filter(B.contains)
      if (remainA == Nil || remainB == Nil) {
        // if remainA is Nil, A is part of the CfR for B.
        // if remainB is Nil, A cannot happen without B happen, which means the node will be removed. So B is a real CfR.
        return false
      } else {
        // neither remainA or remainB is Nil
        // not true that all elements in B precedes every element of A
        !remainB.forall(x => remainA.forall(y => graph.shortestDistance(x, y) != -1)) //&&
        // at least some elements of A precedes elements of B or are parallel. I THIS THIS IS INCLUDED IN THE PREV CONDITION
        // remainA.exists(x => remainB.forall(y => graph.shortestDistance(x, y) != -1))
      }
    }

  /**
   * finds a bottom layer for a list of partially ordered vertices in a graph
   *
   */
  def bottomLayer(graph: Graph, clusters: List[Cluster]): List[Cluster] =
    {
      var temp = clusters
      for (c <- temp) {
        if (clusters.exists(x => graph.shortestDistance(c, x) != -1)) {
          // the path c -> ... -> x exists
          temp = temp.filterNot(_ == c)
        }
      }

      temp
    }
}