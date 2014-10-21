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
  def propFromParent(graph: Graph, map: HashMap[Cluster, List[List[Cluster]]], order: List[Cluster]): (HashMap[Cluster, List[List[Cluster]]], List[(List[Cluster], List[Cluster])]) =
    {

      var raceConditions = List[(List[Cluster], List[Cluster])]()
      for (c <- order if graph.parentsOf(c) != Nil) {
        println("processing " + c.name)
        val parents = graph.parentsOf(c)

        val allPossibleAssignment = assignParents(c, parents, graph, map)
        println(c)
        println(allPossibleAssignment)
        var list = map.getOrElse(c, List[List[Cluster]]())
        var newCfR = List[List[Cluster]]()
        for (assignment <- allPossibleAssignment) {
          // filtering bad solutions and collect correct solutions
          val answer = collectFeasible(assignment, graph, map)
          println("answer =" + answer)
          if (answer._1 != Nil) {
            newCfR = answer._1 ::: newCfR
          }
          if (answer._2 != Nil) {
            raceConditions = answer._2 ::: raceConditions
          }
        }

        val cfrList = simplify((newCfR ::: list).distinct)
        map.update(c, cfrList)
      }

      (map, raceConditions)

    }

  def assignParents(c: Cluster, parents: List[Cluster], graph: Graph, map: HashMap[Cluster, List[List[Cluster]]]): List[(ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster])] =
    {

      val tuple = (ListBuffer[Cluster](), ListBuffer[Cluster](), ListBuffer[Cluster]())
      var curList = ListBuffer[(ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster])]()
      curList += tuple

      for (p <- parents) {
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

      println("update " + p + " with  " + intersection)
      group2cfr.update(p, intersection)

      return true
    }

  def isCaseThree(c: Cluster, graph: Graph, map: HashMap[Cluster, List[List[Cluster]]]): Boolean =
    {
      graph.parentsOf(c) != Nil && map.contains(c) && map(c).exists(_.size == 1)
    }

  def collectFeasible(grouping: (ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster]), graph: Graph, hashmap: HashMap[Cluster, List[List[Cluster]]]): (List[List[Cluster]], List[(List[Cluster], List[Cluster])]) =
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
        println("cfrs in group 3. Do you see one cfr shared by all vertices? " + c4g3)
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
        println("cfr for " + p + "is " + cfr)
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
        var raceConditions = List[(List[Cluster], List[Cluster])]()
        for (p <- pairs) {
          val after = p._1.head
          val before = p._2
          val passTest = before.forall(x => graph.shortestDistance(x, after) > 0)
          println("check ordering: " + passTest)
          if (passTest) {
            good = (p._1 ::: p._2) :: good
          } else {
            // if we don't pass this test, there is a race condition we must note
            raceConditions = (p._1, p._2) :: raceConditions
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
    val graph = CfRSample.graph6().graphWithOptionalsAndSkips
    graph.draw("aaaa")
    var map = immediateMutex(graph)
    println(formatMap(map))
    val order = graph.topoSort
    val answer = propFromParent(graph, map, order)
    map = answer._1
    val raceConditions = answer._2
    println(formatMap(map))
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
    println(simplify(list))
  }

  def processGraph(graph: Graph) =
    {
      init()
      var map = immediateMutex(graph.graphWithOptionalsAndSkips)
      val order = graph.topoSort
      val answer = propFromParent(graph, map, order)
      answer
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

}