package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.nlp._

object SentenceSelectionFunctions {

  /**
   * the heuristic evaluates two adjacent sentences
   *  +1 for every overlapping noun, and -1 for every overlapping verb
   *  stopwords are filtered
   */
  def adjacentHeuristic(prev: Sentence, cluster: Cluster): List[(Sentence, Double)] =
    {
      val prevTokens = prev.tokens.filterNot(t => StopWordStore.isStopWord(t.word))
      val nouns = prevTokens.filter(_.pos.startsWith("N"))
      val verbs = prevTokens.filter(_.pos.startsWith("VB"))
      //println("verbs: " + verbs.mkString("(", ", ", ")"))
      
      cluster.members.map {
        current =>
          val currTokens = current.tokens.filterNot(t => StopWordStore.isStopWord(t.word))
          //println("current toks: " + currTokens.mkString("(", ", ", ")"))
          val repeatedNouns = currTokens.filter(tok => tok.pos.startsWith("N") && nouns.exists(n => n.word == tok.word))
          // for verbs, we only require the lemma to be identical
          val repeatedVerbs = currTokens.filter(tok => tok.pos.startsWith("VB") && verbs.exists(n => n.lemma == tok.lemma))
          //println("repeated nouns " + repeatedNouns.mkString("(", ", ", ")") + "repeated verbs " + repeatedVerbs.mkString("(", ", ", ")") )
          val value = repeatedNouns.size - repeatedVerbs.size

          (current, value.toDouble)
      }
    }

  def reciprocalRank(cluster: Cluster, func: Sentence => Double): List[(Sentence, Double)] = {
    val list = cluster.members.map(s => (s, func(s)))
    val sortedList = list.sortWith(_._2 < _._2)

    var i = 0
    for (item <- sortedList) yield {
      i += 1
      (item._1, 1.0 / i)
    }
  }

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
  def harmonicMeanRank(cluster: Cluster, func1: Sentence => Double, func2: Sentence => Double): List[(Sentence, Int)] =
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