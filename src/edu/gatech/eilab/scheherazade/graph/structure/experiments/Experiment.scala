package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object Experiment {

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
  def propFromParent(graph: Graph, map: HashMap[Cluster, List[List[Cluster]]], order: List[Cluster]): HashMap[Cluster, List[List[Cluster]]] =
    {
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
          val answer = isFeasible(assignment, graph, map)
          println("answer =" + answer)
          if (answer != Nil) {
            newCfR = answer ::: newCfR
          }
        }
        map.update(c, newCfR ::: list)
      }
      map
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
      queue.enqueue(grandparents:_*)
      
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
        queue.enqueue(pred:_*)

      }

      return true
    }

  def isCaseThree(c: Cluster, graph: Graph, map: HashMap[Cluster, List[List[Cluster]]]): Boolean =
    {
      graph.parentsOf(c) != Nil && map.contains(c) && map(c).exists(_.size == 1)
    }

  def isFeasible(grouping: (ListBuffer[Cluster], ListBuffer[Cluster], ListBuffer[Cluster]), graph: Graph, hashmap: HashMap[Cluster, List[List[Cluster]]]): List[List[Cluster]] =
    {

      // check for empty list
      if (grouping._1.size == 0 && grouping._2.size == 0 && grouping._3.size == 0) {
        return Nil
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
            if (intersection == Nil) return Nil
          }
          results = intersection
        } else {
          return Nil // nothing with size == 1
        }
      }

      // check group 2
      val g2 = grouping._2
      var bigIntersection: List[List[Cluster]] = Nil
      for (p <- g2) {
        val cause4p = hashmap(p)
        val grandparents = graph.parentsOf(p)
        var intersection: List[List[Cluster]] = cause4p
        for (gp <- grandparents) {
          val cause4gp = hashmap(gp)
          // if there does not exist a cfr for p that is a superset of one cfr for gp, return false
          intersection = intersection.filter(c4p => cause4gp.exists(c4gp => c4gp.forall(c4p.contains)))
        }

        if (bigIntersection == Nil) {
          bigIntersection = intersection // initialization
        } else {
          bigIntersection = bigIntersection.intersect(intersection)
          if (bigIntersection == Nil) {
            return Nil
          }
        }
      }

      // collect group 1
      val g1 = grouping._1
      var g1result: List[List[Cluster]] = Nil

      if (g1 != Nil) {
        g1result = hashmap(g1.head)
        for (n <- g1.tail) {
          val r = hashmap(n)
          g1result = g1result.zip(r).map(x => x._1 ::: x._2)
        }
      }

      // this is a set product
      if (g1result != Nil && bigIntersection != Nil) {
        bigIntersection = g1result.zip(bigIntersection).map(x => x._1 ::: x._2)
      } else if (g1result != Nil && bigIntersection == Nil) {
        bigIntersection = g1result
      }

      if (results == Nil && bigIntersection != Nil) {
        return bigIntersection
      } else if (results != Nil && bigIntersection == Nil) {
        return results
      } else {
        // cfr in group three must happen after every nodes in group two
        val pairs = results.zip(bigIntersection)
        return pairs.filter {
          p =>
            val after = p._1.head
            val before = p._2
            println("check ordering: " + before.forall(x => graph.shortestDistance(x, after) > 0))
            before.forall(x => graph.shortestDistance(x, after) > 0)
        }.map(x => x._1 ::: x._2)

      }
    }

  //  def setProduct(set1: List[List[Cluster]], set2: List[List[Cluster]]) =
  //    {
  //      set1.zip(set2).map {
  //        z =>
  //          z._1 ::: z._2
  //      }
  //    }

  def main(args: Array[String]) {
    val graph = CRFPropagation.graph4()
    var map = immediateMutex(graph)
    println(formatMap(map))
    val order = graph.topoSort
    map = propFromParent(graph, map, order)
    println(formatMap(map))
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