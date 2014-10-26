package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.gatech.eilab.scheherazade.generation._
import edu.gatech.eilab.scheherazade.main.Global
import java.io._

object Main {

  def main(args: Array[String]) {
    val totalRuns = 1000

    var i = 1
    var noMistake = true
    var total = 0
    var used = 0
    var wrong = 0
    while (i < totalRuns && noMistake) {

      i += 1
      val graph = SampleGraph.randomDAG(10, 30, 4)
      val (background, queryCluster) = generateQuery(graph)
      val (oldTotal, oldValid, newTotal, newValid, correct) = testGraph(graph, background, queryCluster)
      if (correct)
      {
        total += oldTotal
        used += newTotal
      }
      else
      {
        wrong += 1
      }
    }
    
    println("wrong " + wrong)
    println("all ratio " + (used * 1.0 / total) )
  }

  def testGraph(graph: Graph, bgList: List[Cluster], q: Cluster) = {
    val oldSeqs = ExhaustiveSeqGen.generate(graph)

    val oldValid = oldSeqs.filter(seq => seq.contains(q))
    val oldRatio = oldValid.size * 1.0 / oldSeqs.size
    println("# old seqs " + oldSeqs.size + " ratio = " + oldRatio)

    val (newGraph, newSeqs) = Main.performAnalysis(graph, bgList, q)
    val newValid = newSeqs.filter(seq => seq.contains(q))
    val newRatio = newValid.size * 1.0 / newSeqs.size
    println("# new seqs " + newSeqs.size + " ratio = " + newRatio)

    (oldSeqs.size, oldValid, newSeqs.size, newValid, oldRatio == newRatio)
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

    val (cfrMap, raceConds, condPrec) = CfRComputer2.processGraph(graph)
    println(cfrMap)
    println(raceConds)
    println(condPrec)
    val order = graph.topoSort

    // I don;t know if we need to delete the vertices in a particular order
    val removals = cfrMap.filter(pair => pair._2.exists(cfr => cfr.allVertices.forall(backgrounds.contains)))
      .map(x => x._1).toList.distinct
    val race = raceConds.filter(rc => rc.a.exists(backgrounds.contains) || rc.b.exists(backgrounds.contains))
    val noRemovals = race.map(_.focus)

    val linksToAdd = condPrec.filter(_.before.size == 1)
    val conditionsToTest = condPrec.filter(_.before.size > 1)
    val rList = removals.filterNot(noRemovals.contains)
    val newGraph = makeNewGraph(graph, rList, linksToAdd)

    println("removed " + rList.size + rList)
    val newSeqs = ExhaustiveSeqGen.generate(newGraph).filter(s => meetsCondition(s, conditionsToTest))
    (newGraph, newSeqs)
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

  private def makeNewGraph(graph: Graph, removals: List[Cluster], linksToAdd: List[ConditionalPrec]): Graph =
    {

      val graph1 = graph.graphWithOptionalsAndSkips.addSkipLinks(removals).removeNodes(removals)
      val temporalLinks = linksToAdd.map {
        condprec => new Link(condprec.before.head, condprec.after, "T")
      }
      val newLinks = temporalLinks ::: graph1.links.filterNot(l => removals.contains(l.source) || removals.contains(l.target))

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