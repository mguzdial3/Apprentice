package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

/**
 * with new insight Oct 23 2014
 *
 */
object CfRComputer2 {

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
  def immediateMutex(graph: Graph): HashMap[Cluster, List[Cluster]] =
    {
      val map = HashMap[Cluster, List[Cluster]]()
      for (mx <- graph.mutualExcls) {
        val c1 = mx.c1
        val c2 = mx.c2

        var c1List = map.getOrElse(c1, List[Cluster]())
        c1List = c2 :: c1List
        map.update(c1, c1List)

        var c2List = map.getOrElse(c2, List[Cluster]())
        c2List = c1 :: c2List
        map.update(c2, c2List)
      }

      map
    }

  def processCfR(graph: Graph, order: List[Cluster]): (HashMap[Cluster, List[CauseForRemoval]], List[RaceCondition]) =
    {
      val cfrMap = HashMap[Cluster, List[CauseForRemoval]]()
      var allRaceList = List[RaceCondition]()
      val mutexMap = immediateMutex(graph)

      for (c <- order) {
        val parents = graph.parentsOf(c)

        if (parents == Nil) {
          if (mutexMap.contains(c)) {
            val mutexList = mutexMap(c)
            val cfrList = mutexList.map {
              mxVertex => new CauseForRemoval(null, List(mxVertex))
            }
            cfrMap.update(c, cfrList)
          }
        } else {

          val (cfrList, raceList) = mergeParentsCfR(c, parents, cfrMap, graph)
          allRaceList = raceList ::: allRaceList

          var newCfrList = cfrList
          if (mutexMap.contains(c)) {
            val mutexList = mutexMap(c)

            for (mxVertex <- mutexList) {

              // if mxVertex already deletes all parents of c
              val deletingAllParents = cfrList.exists(cfr => cfr.allVertices.contains(mxVertex))
              if (!deletingAllParents) {
                newCfrList = new CauseForRemoval(mxVertex, Nil) :: newCfrList
              }
            }
          }
          
          var finalList = newCfrList.filterNot(x => newCfrList.exists(y => x != y && x.strictSuperSetOf(y)))
          cfrMap.update(c, finalList)
        }
      }

      (cfrMap, allRaceList)
    }

  /**
   * returns a list of CauseForRemoval and a list of RaceConditions
   *
   */
  def mergeParentsCfR(focus: Cluster, parentList: List[Cluster], cfrMap: HashMap[Cluster, List[CauseForRemoval]], graph: Graph): (List[CauseForRemoval], List[RaceCondition]) =
    {
//      if (focus.name == "i") {
//        println("processing i")
//      }
      
      var parents = parentList
      var cfrList = List[CauseForRemoval]()
      var raceList = List[RaceCondition]()

      if (cfrMap.contains(parents.head)) {
        var activeCfRs = cfrMap(parents.head)
        parents = parents.tail
        while (parents != Nil && activeCfRs != Nil) {
          var newActiveCfRs = List[CauseForRemoval]()
          val nextCfRs = cfrMap(parents.head)
          parents = parents.tail

          for (currentCfR <- activeCfRs) {
            for (next <- nextCfRs) {
              val compatiblity = next.compatibleWith(graph, currentCfR)
              if (compatiblity == CauseForRemoval.COMPATIBLE) {
                newActiveCfRs = next.mergeWith(currentCfR) :: newActiveCfRs
              } else if (compatiblity == CauseForRemoval.RACE_CONDITION) {
                if (!currentCfR.raceCondition) {
                  newActiveCfRs = next.mergeRaceCondition(currentCfR) :: newActiveCfRs
                } else {
                  val raceCond = new RaceCondition(focus, currentCfR.allVertices, next.allVertices)
                  raceList = raceCond :: raceList
                }
              }
            }
          }

          activeCfRs = newActiveCfRs
        }

        // make sure to collect those race conditions
        val fakeCfRs = activeCfRs.filter(_.raceCondition).map(x => new RaceCondition(focus, List(x.group3), x.other)) // must have a group 3 because it causes the race condition
        raceList = fakeCfRs ::: raceList
        cfrList = activeCfRs.filter(!_.raceCondition)
      }

      // needs to check if the race condition can indeed remove every parent vertex. Or is this check necessary?
      raceList = raceList.filter { cond =>
        val allClusters = cond.a ::: cond.b
        parents.forall {
          p => // for all parents, there is one cfr for each parent that's included in this race condition
            cfrMap.contains(p) && cfrMap(p).exists(cfr => cfr.allVertices.forall(allClusters.contains))
        }
      }

      (cfrList, raceList)
    }

  def setProduct(set1: List[List[Cluster]], set2: List[List[Cluster]]) =
    {
      for (x <- set1; y <- set2) yield (x ::: y).distinct
    }
  
  //TODO: missing the race conditions caused by two group 3 vertices.

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

    //println(formatMap(map))
    val order = graph.topoSort

    val answer = processCfR(graph, order)
    val map = answer._1
    val raceConditions = answer._2
    println(formatMap(map))
    println(raceConditions)
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

  def processGraph(graph: Graph): (HashMap[Cluster, List[CauseForRemoval]], List[RaceCondition]) =
    {
      init()
      val g = graph.graphWithOptionalsAndSkips
      //println(formatMap(map))
      val order = g.topoSort

      val answer = processCfR(g, order)
      val map = answer._1
      val raceConditions = answer._2

      answer
    }

  def formatMap(map: HashMap[Cluster, List[CauseForRemoval]]): String =
    {
      val str = new StringBuffer()
      for (key <- map.keySet) {
        str.append(key.name)
        str.append(" -> { ")
        for (value <- map(key)) {
          str.append(value.toString)
        }
        str.append(" }, ")
      }

      str.toString
    }

}