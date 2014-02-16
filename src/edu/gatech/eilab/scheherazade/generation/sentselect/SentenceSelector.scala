package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._

import scala.collection.mutable.ListBuffer

/**
 * this class selects sentences based on a Markov model
 *  It takes two functions as parameters: evalSentence, which returns the weights for individual sentences in a cluster.
 *  evalAjacentSent, which returns the preference for a pair of sentences
 */
class SentenceSelector(evalSentence: Cluster => List[(Sentence, Double)], evalAdjacentSent: (Sentence, Cluster) => List[(Sentence, Double)]) {

  

  /**
   * A Viterbi-like algorithm for finding best sentence sequence
   *  The current version does not normalize the probabilities, which assumes the absolute values matter.
   *  The probability normalization assumes only the relative proportions matter.
   *  This assumption is subjective to tuning.
   *  The objective function is assumed to be a product of the weights.
   */
  def bestSentenceSequence(clusters: List[Cluster]): List[Sentence] = {
    var bestSequences = List[(List[Sentence], Double)]()

    // initialization   
    bestSequences = evalSentence(clusters(0)).map { x => (List(x._1), x._2) }

    // one event by one event
    for (nextCluster <- clusters.tail) {

      var allSequences = ListBuffer[(List[Sentence], Double)]()
      var nextBestSequences = ListBuffer[(List[Sentence], Double)]()

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

    val bestSeq = bestSequences.maxBy(_._2)
    bestSeq._1.reverse
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