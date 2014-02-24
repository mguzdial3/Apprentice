package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.nlp._

object SentenceSelectionFunctions {

  /**
   * perform an exponential operation before doing the average, which
   *  means that extreme values are given higher influence so that they
   *  are not masked by a large number of small values
   */
  def exponentialAverage(values: List[Double], alpha: Double): Double =
    {
      val sum = values map {
        v: Double =>
          if (v != 0) {
            val abs = math.abs(v)
            val sign = v / abs
            math.exp(alpha * abs) * sign
          } else 0
      } sum

      sum / values.size
    }

  /**
   * the heuristic evaluates two adjacent sentences
   *  +1 for every overlapping noun, and -1 for every overlapping verb
   *  stopwords are filtered
   */
  def adjacentHeuristic(prev: SingleDescription, cluster: ClusterLike): List[(SingleDescription, Double)] =
    {
      val prevTokens = prev.allTokens.filterNot(t => StopWordStore.isStopWord(t.word))
      val nouns = prevTokens.filter(_.pos.startsWith("N"))
      val verbs = prevTokens.filter(_.pos.startsWith("VB"))
      //println("verbs: " + verbs.mkString("(", ", ", ")"))

      cluster.members.map {
        current =>
          val currTokens = current.allTokens.filterNot(t => StopWordStore.isStopWord(t.word))
          //println("current toks: " + currTokens.mkString("(", ", ", ")"))
          val repeatedNouns = currTokens.filter(tok => tok.pos.startsWith("N") && nouns.exists(n => n.word == tok.word))
          // for verbs, we only require the lemma to be identical
          val repeatedVerbs = currTokens.filter(tok => tok.pos.startsWith("VB") && verbs.exists(n => n.lemma == tok.lemma))
          //println("repeated nouns " + repeatedNouns.mkString("(", ", ", ")") + "repeated verbs " + repeatedVerbs.mkString("(", ", ", ")") )
          val value = repeatedNouns.size / (repeatedVerbs.size + 0.5) + 1

          (current, value.toDouble * 2)
        //(current, 1.0)
      }
    }

  def reciprocalRank(cluster: ClusterLike, func: SingleDescription => Double): List[(SingleDescription, Double)] = {
    val list = cluster.members.map(s => (s, func(s)))
    val sortedList = list.sortWith(_._2 < _._2)

    var i = 0
    for (item <- sortedList) yield {
      i += 1
      (item._1, 1.0 / i)
    }
  }

  def rank(cluster: ClusterLike, func: SingleDescription => Double): List[(SingleDescription, Int)] = {
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
  def harmonicMeanRank(cluster: ClusterLike, func1: SingleDescription => Double, func2: SingleDescription => Double): List[(SingleDescription, Int)] =
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