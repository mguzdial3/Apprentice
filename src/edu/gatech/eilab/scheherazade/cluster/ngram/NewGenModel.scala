package edu.gatech.eilab.scheherazade.cluster.ngram
import java.io._
/**
 * Each cluster is now modelled as a multinomial distribution instead of a dirichlet distribution
 * This is based on GenModel
 *
 */
import breeze.stats.distributions._
import breeze.linalg.DenseVector
/**
 * a n-gram generative model for clustering sentences
 *
 */
object NewGenModel {

  val SMALL = 1e-8

  def train(corpus: NGramCorpus): Array[Int] = {

    val numSents = corpus.ngrams.length
    // number of unique ngrams
    val numNgrams = corpus.vectors.keys.size

    // manually set parameters
    val numClusters = 160
    val dimension = 1000

    val numIterations = 100

    //    println(components.distinct.size)
    //System.exit(1)

    /** random initialization **/
    // Each sentence has a Y variable, which selects a cluster
    var y = DenseVector.rand(numSents, Rand.randInt(numClusters))

    // each cluster has a PI variable representing a multinomial distribution over the topics in this cluster. 
    // It must sum up to 1.  
    var pi = Array.fill(numClusters) {
      val v = DenseVector.rand(dimension)
      v / v.sum
    }

    // topic selection for each ngram
    var z = Array.ofDim[DenseVector[Int]](numSents)

    for (i <- 0 until numSents) {
      z(i) = DenseVector.rand(corpus.ngrams(i).length, Rand.randInt(dimension))
    }

    val m = DenseVector.zeros[Int](numSents)
    for (i <- 0 until numSents) {
      m(i) = z(i).size // number of ngrams in the sentence
    }

    // q(z)
    var q = Array.fill(numSents, m.max)(DenseVector.zeros[Double](dimension))

    //val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("print.txt")))
    var diverseY = Array.fill(numSents)(0)
    var used = 0
    for (iter <- 1 to numIterations) {

      println("iteration: " + iter)
      val oldY = y.copy

      //pw.println("iteration: " + iter)
      /** E-Step **/
      /** Step 1: computing y = argmax P(x,z|y) **/
      for (i <- 0 until numSents) {
        // outer loop for each sentence
        var maxY = -1
        var maxSum = Double.NegativeInfinity

        for (d <- 0 until numClusters) {
          var sum = 0.0
          for (j <- 0 until m(i)) {
            val text = corpus.ngrams(i)(j)
            val textVector = corpus.vectors(text)
            val s = textVector dot pi(d) // summing over possible values of z_ij
            sum += math.log(s) // take log and sum again
          }

          if (sum > maxSum) {
            maxSum = sum
            maxY = d
          }
        }

        y(i) = maxY
        //println(y(i))
      }

      val distinctY = y.toArray.toList.distinct
      println("non-empty clusters:" + distinctY.mkString(" ") + " (" + distinctY.size + ")")
      if (distinctY.size >= used) {
        used = distinctY.size
        diverseY = y.toArray
      }

      if ((oldY - y).norm(1) < 0.01 || iter == numIterations) {
        // converged or max iteration reached
        return y.toArray
      }

      /** finding q(z_ij) **/

      for (i <- 0 until numSents; j <- 0 until m(i)) {
        val text = corpus.ngrams(i)(j)
        val textVector = corpus.vectors(text)
        q(i)(j) = Utils.normalizeLogProb {
          pi(y(i)).map(math.log) + textVector.map(math.log)
        }

      }

      /*** M-Step ***/

      /** compute Pi **/

      for (c <- 0 until numClusters) {
        val newPi = DenseVector.zeros[Double](dimension)
        for (i <- 0 until numSents if (y(i) == c); j <- 0 until m(i)) {
          newPi += q(i)(j)
        }

        pi(c) = newPi / newPi.sum
        for (k <- 0 until dimension) {
          if (pi(c)(k) < SMALL)
            pi(c)(k) = SMALL
        }
      }

      val empty = (0 until numClusters) filterNot (y.toArray.toList.distinct contains)
      for (c <- empty) {
        // regenerate
        if (math.random < 0.9) {
          val v = DenseVector.rand(dimension)
          pi(c) = v / v.sum
        }
      }

    }

    diverseY.toArray
  }
}