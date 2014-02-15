package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._

object SentenceSelectionFunctions {

  def rank(cluster: Cluster, func: Sentence => Double): List[(Sentence, Int)] = {
    val list = cluster.members.map(s => (s, func(s)))
    val sortedList = list.sortWith(_._2 < _._2)

    var i = 0
    for (item <- sortedList) yield {
      i += 1
      (item._1, i)
    }
  }

  def exponentiateRank(ranks: List[(Sentence, Int)]): List[(Sentence, Double)] =
    ranks.map(r => (r._1, math.exp(-1 * r._2)))

  /**
   * takes two functions, computes their separate ranks and their combined rank using the harmonic mean
   *
   */
  def harmonicMeanRank(cluster: Cluster, func1: Sentence => Double, func2: Sentence => Double) : List[(Sentence, Int)]=
    {
      val rank1 = rank(cluster, func1)
      val rank2 = rank(cluster, func1)

      val list =
        for (sent <- cluster.members) yield {
          val r1 = rank1.find(_._1 == sent).get._2
          val r2 = rank2.find(_._1 == sent).get._2
          val fitness = 2 * r1 * r2 / (r1 + r2)
          (sent, fitness)
        }

      val sortedList = list.sortWith(_._2 < _._2)

      var i = 0
      for (item <- sortedList) yield {
        i += 1
        (item._1, i)
      }
    }
}