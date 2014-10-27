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

  def processCfR(graph: Graph, order: List[Cluster], mutexMap: HashMap[Cluster, List[Cluster]]): (HashMap[Cluster, List[CauseForRemoval]], List[RaceCondition]) =
    {
      val cfrMap = HashMap[Cluster, List[CauseForRemoval]]()
      var allRaceList = List[RaceCondition]()

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

          var finalList = newCfrList.filterNot(x => newCfrList.exists(y => x != y && x.strictSuperSetOf(y))).distinct
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
          val nextCfRs = cfrMap.getOrElse(parents.head, Nil)
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

  /**
   * find the temporal links we need to add to the graph
   *
   */
  def findTemporalLinks(graph: Graph, cfrMap: HashMap[Cluster, List[CauseForRemoval]], order: List[Cluster], mutexMap: HashMap[Cluster, List[Cluster]]): (List[ConditionalPrec], List[RaceCondition]) =
    {
      var allTemporals = List[ConditionalPrec]()
      var allRaces = List[RaceCondition]()
      for (c <- order) {
        //        if (c.name == "f") {
        //          println(c.name)
        //        }
        var counter = 0
        val mutexList = mutexMap.getOrElse(c, Nil)
        val cCfRList = cfrMap.getOrElse(c, Nil)

        val parents = graph.parentsOf(c)
        for (p <- parents if cfrMap.contains(p)) // a parent that can be removed
        {
          var temporals = List[ConditionalPrec]()
          var potentials = List[ConditionalPrec]()

          val cfrList = cfrMap(p)
          for (cfr <- cfrList) {
            val notDeleteC = !cCfRList.exists(x => x == cfr || cfr.strictSuperSetOf(x))
            // No vertex in the CfR is ordered wrt C. It is also not ordered after p. 
            // If it is not ordered before    
            val parallelCheck = cfr.allVertices.forall(x => !graph.ordered(x, c)) && cfr.allVertices.forall(x => graph.shortestDistance(p, x) == -1)
            if (notDeleteC && parallelCheck) {
              temporals = new ConditionalPrec(cfr.allVertices, c) :: temporals
            }
            // even if it is preceding c, it could be optional
            val canSkip = cfr.allVertices.exists(x => graph.shortestDistance(x, c) != -1 && graph.optionals.contains(x)) &&
              cfr.allVertices.forall(x => graph.shortestDistance(c, x) == -1)
            if (notDeleteC && canSkip) {
              potentials = new ConditionalPrec(cfr.allVertices, c) :: potentials
            }
          }

          if (temporals.size == 0) {
            // no race condition possible
          } else if (temporals.size == 1 && potentials.size == 0) {
            allTemporals = temporals ::: allTemporals
          } else if (temporals.size == 1 && potentials.size > 0) {
            // race conditions
            val tList = temporals.head.before
            for (potent <- potentials) {
              val pList = potent.before
              //allRaces = new RaceCondition(c, tList, pList) :: new RaceCondition(p, tList, pList) :: allRaces
              allRaces = new RaceCondition(p, tList, pList) :: allRaces
            }
          } else if (temporals.size > 1) {
            // race conditions between all temporals and between temporals and potentials
            val temporalsCopy = temporals
            for (t1 <- temporals; t2 <- temporalsCopy if t1 != t2) {
              val list1 = t1.before
              val list2 = t2.before
              //allRaces = new RaceCondition(c, list1, list2) :: new RaceCondition(p, list1, list2) :: allRaces
              allRaces = new RaceCondition(p, list1, list2) :: allRaces
            }

            for (t1 <- potentials; t2 <- temporals) {
              val list1 = t1.before
              val list2 = t2.before
              allRaces = new RaceCondition(p, list1, list2) :: allRaces
              //allRaces = new RaceCondition(c, list1, list2) :: new RaceCondition(p, list1, list2) :: allRaces
            }
          }

        }
        //        println(c.name)
        //        println(allTemporals)
        //        println(allRaces)
      }

      (allTemporals.distinct, allRaces.distinct)
    }

  /**
   * to create a race condition, we need two CfRs that are in conflict with each other
   *  (1) They must both have a Cat-B vertex.
   *  (2) The Cat-B vertex is not the same
   *  (3) One deletes the focus vertex by deleting all parents but not the vertex itself, and one deletes some but not all parents.
   *
   */
  def findRacesForCatB(graph: Graph, cfrMap: HashMap[Cluster, List[CauseForRemoval]], order: List[Cluster], mutexMap: HashMap[Cluster, List[Cluster]]): List[RaceCondition] =
    {
      var allRaceConditions = List[RaceCondition]()
      for (c <- order if graph.parentsOf(c) != Nil && cfrMap.contains(c)) // it has some parents and it may be removed from the graph
      {
        val mutexList = mutexMap.getOrElse(c, Nil)
        val selfCfRs = cfrMap(c)
        // first, the CfR does not remove c directly. That is, it works by removing all of its parents. 
        // second, the CfR has to contain a Cat-B vertex.  
        val potentialConflictCfRs = selfCfRs.filter(cfr => cfr.group3 != null && cfr.allVertices.forall(v => !mutexList.contains(v)))
        val parents = graph.parentsOf(c)
        for (p <- parents) {
          for (parentCfR <- cfrMap.getOrElse(p, Nil)) {
            if (parentCfR.group3 != null && // parentCfR has a group 3 
              parentCfR.allVertices.forall(v => !mutexList.contains(v)) && // parentCfR does not remove c directly               
              (!selfCfRs.exists(cfr => cfr != parentCfR && parentCfR.strictSuperSetOf(cfr)))) // it does not delete all parents
              {
              val opponents = potentialConflictCfRs.filter(cfr => (parentCfR != cfr) && (!cfr.strictSuperSetOf(parentCfR)) && (cfr.group3 != parentCfR.group3))
              val raceConds = opponents.map(oppo =>
                new RaceCondition(c, oppo.allVertices, parentCfR.allVertices))
              allRaceConditions = raceConds ::: allRaceConditions
            }
          }
        }
      }

      allRaceConditions
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
    //graph.draw("aaaa")

    //println(formatMap(map))
    val order = graph.topoSort

    val mutexMap = immediateMutex(graph)
    val answer = processCfR(graph, order, mutexMap)
    val cfrMap = answer._1
    val moreRace = findRacesForCatB(graph, cfrMap, order, mutexMap)
    val raceConditions = answer._2 ::: moreRace
    println(formatMap(cfrMap))
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

  def processGraph(graph: Graph): (HashMap[Cluster, List[CauseForRemoval]], List[RaceCondition], List[ConditionalPrec], List[ForcedCooccurence]) =
    {
      init()
      val g = graph.graphWithOptionalsAndSkips
      //      g.draw("real-graph")

      val order = g.topoSort

      val mutexMap = immediateMutex(g)
      val answer = processCfR(g, order, mutexMap)
      val cfrMap = answer._1
      val moreRace = findRacesForCatB(g, cfrMap, order, mutexMap)

      val (condPrec, race2) = findTemporalLinks(g, cfrMap, order, mutexMap)
      val raceConditions = race2 ::: answer._2 ::: moreRace

      val cfList = findForcedCooccurence(g)
      (cfrMap, raceConditions, condPrec, cfList)
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

  def findForcedCooccurence(graph: Graph): List[ForcedCooccurence] = {
    // just used the code that find optional events
    var fcList = List[ForcedCooccurence]()

    // condition 1: c1 and c2 share a mutual exclusion but there is also a path from c1 to c2 on the graph
    val candidates = graph.mutualExcls.filter(m => graph.ordered(m.c1, m.c2)).map(m => (m.c1, m.c2))

    // condition 2: c1 is not mutually exclusive to another (direct or indirect) predecessor of c2
    candidates.foreach {
      case (c1, c2) =>
        var early: Cluster = null
        var late: Cluster = null
        if (graph.shortestDistance(c1, c2) != -1) {
          early = c1
          late = c2
        } else {
          early = c2
          late = c1
        }

        // tests if the early node is mutual exclusive to another node, which has a clear path to the late node            
        // passing the test will prevent the recognition of the optionality.
        val prevented = graph.mutualExcls.find(m =>
          (m.c1 == early && m.c2 != late && graph.shortestDistance(m.c2, early) == -1 && graph.hasClearPath(m.c2, late)) ||
            (m.c2 == early && m.c1 != late && graph.shortestDistance(m.c1, early) == -1 && graph.hasClearPath(m.c1, late)))

        if (prevented.isDefined) {
          val mx = prevented.get
          val c =
            {
              if (mx.c1 == early)
                mx.c2
              else mx.c1
            }
          fcList = new ForcedCooccurence(c, late) :: fcList
        }
    }

    fcList
  }

}