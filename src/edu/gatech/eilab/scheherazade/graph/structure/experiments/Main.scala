package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.gatech.eilab.scheherazade.generation._
import edu.gatech.eilab.scheherazade.main.Global
import java.io._
import scala.actors.Actor

class Ping(id: Int, iter: Int, pong: Pong) extends Actor {
  def act() {
    receive {
      case "Start" =>
        println(id + " started")
        val tuple = Main.compute(iter)
        pong ! tuple
    }
  }
}

class Pong(t: Int) extends Actor {

  var totalWrong = 0
  var totalSim = 0
  var totalOriginal = 0
  var totalNewTime = 0.0
  var totalOldTime = 0.0

  var count = 0

  def act() {
    while (count < t) {
      receive {
        case (wrong: Int, used: Int, total: Int, newTime: Double, oldTime: Double) =>
          println("received data " + count)
          totalWrong += wrong
          totalSim += used
          totalOriginal += total
          totalNewTime = newTime
          totalOldTime = oldTime
          count += 1
      }
    }

    println("wrong " + totalWrong)
    println("seq ratio " + (totalSim * 1.0 / totalOriginal))
    println("time ratio " + totalNewTime / totalOldTime)
  }

}

object Main {

  def main(args: Array[String]) {
    val total = 10000
    val number = 8
    val pong = new Pong(number)
    pong.start

    for (i <- 1 to number) {
      val ping = new Ping(i, total / number, pong)
      ping.start
      ping ! "Start"
    }

  }

  def compute(totalRuns: Int) = {

    var i = 1
    var noMistake = true
    var total = 0
    var totalOldTime = 0.0
    var totalNewTime = 0.0
    var used = 0
    var wrong = 0
    while (i < totalRuns && noMistake) {

      i += 1
      val graph = SampleGraph.randomDAG(16, 70, 6)
      val (background, queryCluster) = generateQuery(graph)
      try {
        val (oldTotal, oldValid, newTotal, newValid, correct, oldTime, newTime) = testGraph(graph, background, queryCluster)

        if (correct) {
          total += oldTotal
          used += newTotal
          totalOldTime += oldTime / 1000.0
          totalNewTime += newTime / 1000.0
        } else {
          wrong += 1
          //        printTestCase(graph, background, queryCluster)
          //        System.exit(1)
        }
      } catch {
        case ex: Exception => wrong += 1
      }
    }

    (wrong, used, total, totalNewTime, totalOldTime)
  }

  def printTestCase(graph: Graph, bgList: List[Cluster], q: Cluster) {
    for (c <- graph.nodes) {
      println("val " + c.name.toLowerCase() + " = new Cluster (\"" + c.name + "\", Nil)")
    }
    println("val nodes = List(" + graph.nodes.map(_.name.toLowerCase()).mkString("", ",", ")"))

    println("val links = List(")
    for (l <- graph.links) {
      println("new Link(" + l.source.name.toLowerCase() + ", " + l.target.name.toLowerCase() + "),")
    }
    println(")")

    println("val mutex = List(")
    for (mx <- graph.mutualExcls) {
      println("new MutualExcl(" + mx.c1.name.toLowerCase() + ", " + mx.c2.name.toLowerCase() + "),")
    }
    println(")")

    println("new Graph(nodes, links, mutex)")

    println()
    println()
    println("val background = graph.nodes(" + graph.nodes.indexOf(bgList.head) + ")")
    println("val queryCluster = graph.nodes(" + graph.nodes.indexOf(q) + ")")
    println("testGraph(graph, List(background), queryCluster)")
  }

  def testGraph(graph: Graph, bgList: List[Cluster], q: Cluster) = {
    var time1 = System.currentTimeMillis()
    val oldSeqs = ExhaustiveSeqGen.generate(graph)
    var time2 = System.currentTimeMillis()
    val oldTime = time2 - time1
    val oldValid = oldSeqs.filter(seq => bgList.forall(seq.contains))
    val oldQueried = oldValid.filter(seq => seq.contains(q))
    val oldRatio = oldQueried.size * 1.0 / oldValid.size
    //println("# old seqs " + oldSeqs.size + " ratio = " + oldRatio)

    time1 = System.currentTimeMillis()
    val (newGraph, newSeqs) = Main.performAnalysis(graph, bgList, q)
    time2 = System.currentTimeMillis()
    val newTime = time2 - time1
    val newValid = newSeqs.filter(seq => bgList.forall(seq.contains))
    val newQueried = newValid.filter(seq => seq.contains(q))
    val newRatio = newQueried.size * 1.0 / newValid.size
    //println("# new seqs " + newSeqs.size + " ratio = " + newRatio)

    val correct =
      {
        (oldRatio.isNaN() && newRatio.isNaN()) || oldRatio == newRatio
      }
    (oldSeqs.size, oldValid, newSeqs.size, newValid, correct, oldTime, newTime)
    //    if (newRatio != oldRatio) {
    //
    //      graph.graphWithOptionalsAndSkips.draw("mutex-old")
    //      newGraph.draw("mutex-new")
    //      val missing = oldSeqs.filterNot(newSeqs.contains)
    //      val extra = newSeqs.filterNot(oldSeqs.contains)
    //      println("missing seqs:")
    //      printSeqs(missing)
    //      println("extra seqs")
    //      printSeqs(extra)
    //    }
  }

  def printSeqs(seqs: List[List[Cluster]]) {
    for (seq <- seqs) {
      println(seq.map(_.name).mkString(" "))
    }
  }

  def performAnalysis(graph: Graph, backgrounds: List[Cluster], qVertex: Cluster): (Graph, List[List[Cluster]]) = {

    val realGraph = graph // regularize(graph)

    val (cfrMap1, raceConds, condPrec, cfList) = CfRComputer2.processGraph(realGraph)
    //    println(cfrMap1)
    //    println(raceConds)
    //    println(condPrec)
    //    println(cfList)

    val cfrMap = cfrMap1 //HashMap[Cluster, List[CauseForRemoval]]()
    //    val g = graph.graphWithOptionalsAndSkips
    //    cfrMap1.foreach(pair => 
    //      if (!g.conditionals.contains(pair._1) && !g.optionals.contains(pair._1))
    //      {
    //        cfrMap.update(pair._1, pair._2)
    //      })
    //    println(cfrMap)

    //    cfrMap1.foreach { pair =>
    //      if (graph.optionals.contains(pair._1) || graph.conditionals.contains(pair._1)) {
    //        val second = pair._2.filterNot(
    //          cfr => cfr.allVertices.exists(x => graph.shortestDistance(pair._1, x) > 0))
    //        cfrMap.update(pair._1, second)
    //      }
    //      else
    //      {
    //        cfrMap.update(pair._1, pair._2)
    //      }
    //    }

    // only use the conditions that are included in the background, i.e. those that are needed.
    var realCond = condPrec.filter(c => c.before.forall(backgrounds.contains))

    // I don;t know if we need to delete the vertices in a particular order
    var removals = cfrMap.filter(pair => pair._2.exists(cfr => cfr.allVertices.forall(backgrounds.contains)))
      .map(x => x._1).toList.distinct
    val race = raceConds.filter(rc => rc.a.exists(backgrounds.contains) || rc.b.exists(backgrounds.contains))
    var noRemovals = race.map(_.focus)

    //val linksToAdd = condPrec.filter(_.before.size == 1)
    //val conditionsToTest = condPrec.filter(_.before.size > 1)
    val (realBg, rList) = findRemoved(cfrMap, raceConds, backgrounds)

    val linksToAdd = realCond.filter(x => x.before.size == 1) // && realBg.contains(x.before.head))
    val conditionsToTest = realCond.filter(_.before.size > 1)

    //val rList = removals.filterNot(noRemovals.contains)

    //*** Must respect the ordering of deletion for CfRs

    val newGraph = makeNewGraph(graph, rList, linksToAdd, Nil)
    //newGraph.draw("analysis-new")
    //println("links " ) ; newGraph.links.foreach(l => println(l))
    //println("removed " + rList.size + rList)
    //edu.gatech.eilab.scheherazade.graph.passage.Passage.debug = true
    val newSeqs = ExhaustiveSeqGen.generateFromOld(newGraph, graph).filter(s => meetsCondition(s, conditionsToTest) && meetsFC(s, cfList))
    (newGraph, newSeqs)
  }

  def findRemoved(cfrMap: HashMap[Cluster, List[CauseForRemoval]], raceConds: List[RaceCondition], backgrounds: List[Cluster]): (List[Cluster], List[Cluster]) =
    {

      var forbiddenBg = List[Cluster]()
      for (entry <- cfrMap; cfr <- entry._2) {
        if (cfr.other != Nil && cfr.group3 != null && backgrounds.contains(cfr.group3) && cfr.other.exists(o => !backgrounds.contains(o))) {
          forbiddenBg = cfr.group3 :: forbiddenBg
        }
      }
      //println("forbidden " + forbiddenBg)

      val realBg = backgrounds.filterNot(forbiddenBg.contains)
      //println("realBg = " + realBg)

      var removals = cfrMap.filter(pair => pair._2.exists(cfr => cfr.allVertices.forall(realBg.contains)))
        .map(x => x._1).toList.distinct

      val race = raceConds.filter(rc => rc.a.exists(realBg.contains) || rc.b.exists(realBg.contains))
      var noRemovals = race.map(_.focus)

      val rList = removals.filterNot(noRemovals.contains)
      //println("rList "+rList) 
      (realBg, rList)
    }

  def meetsFC(seq: List[Cluster], fcList: List[ForcedCooccurence]): Boolean =
    {
      for (fc <- fcList) {
        if (seq.contains(fc.dependant) && !seq.contains(fc.precursor)) {
          return false
        }
      }
      return true
    }

  def regularize(graph: Graph): Graph =
    {
      val sources = graph.findSources
      val ends = graph.findEnds
      val startNode = new Cluster("Start", Nil)
      val endNode = new Cluster("End", Nil)

      val newNodes = startNode :: endNode :: graph.nodes

      var newMutex = graph.mutualExcls
      for (s1 <- ends; s2 <- ends if s1 != s2) {
        newMutex = new MutualExcl(s1, s2) :: newMutex
      }

      var newLinks = graph.links
      for (s <- sources) {
        newLinks = new Link(startNode, s) :: newLinks
      }
      for (e <- ends) {
        newLinks = new Link(e, endNode) :: newLinks
      }

      new Graph(newNodes, newLinks.distinct, newMutex.distinct, graph.optionals, graph.conditionals)

    }

  def meetsCondition(seq: List[Cluster], conditionsToTest: List[ConditionalPrec]): Boolean =
    {
      for (condition <- conditionsToTest) {
        val all = condition.before.size
        var count = 0
        var tooEarly = false
        for (c <- seq) {
          // go through the seq once and figure it out
          if (condition.before.contains(c)) {
            count += 1
          } else if (condition.after == c && count < all) {
            tooEarly = true
          }
        }
        if (count == all && tooEarly) {
          return false
        }
      }
      true
    }

  private def makeNewGraph(graph: Graph, removals: List[Cluster], linksToAdd: List[ConditionalPrec], linksToAvoid: List[Link]): Graph =
    {

      val graph1 = graph.graphWithOptionalsAndSkips.addSkipLinks(removals).removeNodes(removals)
      val temporalLinks = linksToAdd.map {
        condprec => new Link(condprec.before.head, condprec.after, "T")
      }
      val newLinks = (temporalLinks ::: graph1.links).filterNot(l => removals.contains(l.source) || removals.contains(l.target) || linksToAvoid.contains(l))

      new Graph(graph1.nodes, newLinks, graph1.mutualExcls, graph1.optionals, graph1.conditionals)
    }

  /**
   * randomly generates a query for the story understanding problem
   *  A query contains one event that must occur, and another event whose probability we want
   */
  def generateQuery(graph: Graph) =
    {
      var meNodes = graph.mutualExcls.flatMap(me => List(me.c1, me.c2)).distinct
      val ind1 = math.floor(math.random * meNodes.size).toInt
      val background = meNodes(ind1)
      meNodes = meNodes.filterNot(_ == background)

      var ind2 = math.floor(math.random * meNodes.size).toInt
      val target = meNodes(ind2)

      (List(background), target)
    }
}