package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._

import scala.collection.mutable.ListBuffer
import edu.gatech.eilab.scheherazade.nlp._
/**
 * this class selects sentences based on a Markov model
 *  It takes two functions as parameters: evalSentence, which returns the weights for individual sentences in a cluster.
 *  evalAjacentSent, which returns the preference for a pair of sentences
 */
class SentenceSelector(evalSentence: ClusterLike => List[(SingleDescription, Double)], evalAdjacentSent: (SingleDescription, ClusterLike) => List[(SingleDescription, Double)]) {

  /**
   * A Viterbi-like algorithm for finding best sentence sequence
   *  The current version does not normalize the probabilities, which assumes the absolute values matter.
   *  The probability normalization assumes only the relative proportions matter.
   *  This assumption is subjective to tuning.
   *  The objective function is assumed to be a product of the weights.
   */
  def bestSentenceSequence(clusters: List[ClusterLike], idf:InverseSentFreq): List[String] = {
    var bestSequences = List[(List[SingleDescription], Double)]()

    // initialization   
    bestSequences = evalSentence(clusters(0)).map { x => (List(x._1), x._2) }

    // one event by one event
    for (nextCluster <- clusters.tail) {

      var allSequences = ListBuffer[(List[SingleDescription], Double)]()
      var nextBestSequences = ListBuffer[(List[SingleDescription], Double)]()

      val sentFit = evalSentence(nextCluster)

      // compute all possible combinations
      for (oldSeq <- bestSequences) {
        val adjSentFit = evalAdjacentSent(oldSeq._1.head, nextCluster)
        for (sf <- adjSentFit) {
          val fitness = oldSeq._2 + math.log(sf._2) + math.log(sentFit.find(_._1 == sf._1).get._2)
          allSequences += ((sf._1 :: oldSeq._1, fitness))
        }
      }

      // find the best for each sentence in nextCluster
      val groups = allSequences.groupBy(_._1.head)
      for (pair <- groups) {
        val bestSoFar = pair._2.maxBy(_._2)
        nextBestSequences += bestSoFar
      }

      // update; get ready for the next event cluster
      bestSequences = nextBestSequences.toList
    }

    val best = bestSequences.maxBy(_._2)
    val bestSeq = best._1.reverse
    
    println("count = " + bestSequences.filter(_._2 == best._2).size)

    println("******** Best Sequence ***********")

    for (i <- 0 to bestSeq.size - 2) {
      println(bestSeq(i).toText)

      val tok1 = bestSeq(i).allTokens.filterNot(t => StopWordStore.isStopWord(t.word))
      val tok2 = bestSeq(i+1).allTokens.filterNot(t => StopWordStore.isStopWord(t.word))
      //println("current toks: " + currTokens.mkString("(", ", ", ")"))
      val repeatedNouns = tok1.filter(tok => tok.pos.startsWith("N") && tok2.filter(t => t.pos.startsWith("N")).exists(t => t.word == tok.word))
      // for verbs, we only require the lemma to be identical
      val repeatedVerbs = tok1.filter(tok => tok.pos.startsWith("VB") && tok2.filter(t => t.pos.startsWith("VB")).exists(t => t.lemma == tok.lemma))
      println("repeated nouns " + repeatedNouns.mkString("(", ", ", ")") + "repeated verbs " + repeatedVerbs.mkString("(", ", ", ")") )
      //val value = (repeatedNouns.size + 0.5) / (repeatedVerbs.size + 0.5)
       val value = (repeatedNouns.map(x => math.log(idf.freq(x)) * -1).sum + 0.5) / (repeatedVerbs.map(x => math.log(idf.freq(x)) * -1).sum + 0.5)

      println("score = " + value)
    }

    bestSeq.map(_.toText)
  }

  /**
   * first do the exp function and then normalize to sum to 1.
   *
   */
  private def normalizeExpProb(weights: Seq[Double]): Seq[Double] = {

    val temp = weights.map {
      w =>
        math.exp(-w)
    }

    val sum = temp.sum
    temp.map(_ / sum)
  }
}