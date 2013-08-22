package edu.gatech.eilab.scheherazade.cluster.ngram

/**
 * generative model for ngram data
 *
 */
import breeze.stats.distributions._
import breeze.linalg.DenseVector
/**
 * a n-gram generative model for clustering sentences
 *
 */
object GenModel {
  val MAGIC_DIVISOR = 0.02

  /**
   * Normalize a distribution so that the sum of all elements equals one.
   * This method is destructive.
   *
   */
  protected def normalize(dist: Array[Double]) {
    val n = dist.length
    var sum = 0.0
    for (j <- 0 until n) {
      sum += dist(j)
    }

    for (j <- 0 until n) {
      dist(j) = dist(j) / sum
    }
  }

  /**
   * generates n DenseVectors which will be used as parameters for Dirichlet distributions
   *
   */
  def generateDirichlet(components: List[Int], dimension: Int, n: Int) =
    {
      val small = 1E-5
      val total = 1 / MAGIC_DIVISOR - (dimension - components.size) * small

      val base = DenseVector.zeros[Double](dimension)
      val range = (0 until dimension).filterNot(components contains)
      for (i <- range) {
        base(i) = small
      }

      for (i <- 1 to n) yield {
        val vector = DenseVector.zeros[Double](dimension)
        var sum = 0.0
        for (i <- components) {
          val v = Rand.uniform.draw
          vector(i) = v
          sum += v
        }

        vector / sum * total + base
      }
    }

  def train(corpus: NGramCorpus): Array[Int] = {

    val numSents = corpus.ngrams.length
    // number of unique ngrams
    val numNgrams = corpus.vectors.keys.size

    // manually set parameters
    val numClusters = 30
    val topicsPerCluster = 5
    val dimension = 1000

    val numIterations = 3

    /** find important components of all vectors **/
    var components = List[Int]()
    for ((_, vector) <- corpus.vectors) {
      for (i <- 0 until vector.size) {
        if (vector(i) > 0.001) {
          components = i :: components
        }
      }
    }

    components = components.distinct
    println(components.distinct)
    //    println(components.distinct.size)
    //System.exit(1)

    /** random initialization **/
    // Each sentence has a Y variable, which selects a cluster
    var y = DenseVector.rand(numSents, Rand.randInt(numClusters))

    // each cluster has a PI variable representing a multinomial distribution over the topics in this cluster. 
    // It must sum up to 1.  
    var pi = Array.fill(numClusters) {
      val v = DenseVector.rand(topicsPerCluster)
      v / v.sum
    }

    // parameter for Dirichlet
    var theta = Array.fill(numClusters) {
      generateDirichlet(components, dimension, topicsPerCluster).toArray
      //      val v = DenseVector.rand(dimension)
      //      v / (v.sum * MAGIC_DIVISOR)
    }

    // topic selection for each ngram
    var z = Array.ofDim[DenseVector[Int]](numSents)

    for (i <- 0 until numSents) {
      z(i) = DenseVector.rand(corpus.ngrams(i).length, Rand.randInt(topicsPerCluster))
    }

    import java.io._
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("print.txt")))

    for (iter <- 1 to numIterations) {

      println("iteration: " + iter)
      pw.println("iteration: " + iter)
      /** E-Step **/
      //val ADD_FACTOR = 800
      // computing y = argmax P(x,z|y)
      for (i <- 0 until numSents) {
        // outer loop for each sentence

        var maxY = -1

        var maxProduct = Double.NegativeInfinity
//        if (z(i).size == 0) {
//          println("num of ngrams = " + z(i).size)
//          System.exit(1)
//        }
        for (j <- 0 until numClusters) {
          // loop for each cluster
          var product = 0.0

          for (ngIdx <- 0 until z(i).size) {
            // for each ngram in the sentence
            val text = corpus.ngrams(i)(ngIdx)

            val textVector = corpus.vectors(text)
            var max = Double.NegativeInfinity
            for (k <- 0 until topicsPerCluster) {
              // find the best assignment to z

              val diri = new Dirichlet(theta(j)(k))
              val multinomial = new Multinomial(pi(j))
              val p = diri.logPdf(textVector) + multinomial.logProbabilityOf(k)

              if (p > max) {
                max = p
              }
            }

            product += max
          }

          if (product > maxProduct) {
            maxProduct = product
            maxY = j
          }
        }
        //println(maxProduct)
        y(i) = maxY
        println(y(i))
      }

      /** finding z **/
      /*
      for (i <- 0 until numSents) {
        val length = corpus.ngrams(i).length
        for (j <- 0 until length) {
          var max = Double.NegativeInfinity
          var maxId = 0

          val text = corpus.ngrams(i)(j)

          val textVector = corpus.vectors(text)

          for (k <- 0 until topicsPerCluster) {
            val diri = new Dirichlet(theta(y(i))(k))
            val prob = diri.logPdf(textVector)

            if (prob > max) {
              max = prob
              maxId = k
            }
          }

          z(i)(j) = maxId

        }
      }
*/
      /** finding q(z) **/

      // initialization of the qz array
      // four levels: numSents, ngrams in that sentence, different possible values of z
      val qz = Array.ofDim[Array[Array[Double]]](numSents)
      for (i <- 0 until numSents) {
        qz(i) = Array.ofDim[Double](corpus.ngrams(i).length, topicsPerCluster)
      }

      // computing qz
      var sum = 0.0
      var count = 0
      var max = Double.NegativeInfinity

      for (i <- 0 until numSents) {
        val multi = Multinomial(pi(y(i)))
        for (j <- 0 until corpus.ngrams(i).length) {
          val textVector = corpus.vectors(corpus.ngrams(i)(j))

          for (k <- 0 until topicsPerCluster) {
            val diri = new Dirichlet(theta(y(i))(k))
            val prob = multi.logProbabilityOf(k) + diri.logPdf(textVector)
            qz(i)(j)(k) = prob
            sum += prob
            count += 1
            if (prob > max)
            {
              max = prob
            }
          }
        }
      }

      val average = sum / count
      //println(average)

      for (i <- 0 until numSents) {
        for (j <- 0 until corpus.ngrams(i).length) {
          for (k <- 0 until topicsPerCluster) {
            qz(i)(j)(k) -= average
            //println(qz(i)(j)(k))
          }
        }
      }

      /*** M-Step ***/

      /** compute theta **/
      pw.println("THETAs")

      for (i <- 0 until numClusters) {
        for (p <- 0 until topicsPerCluster) {

          var newTheta = DenseVector.zeros[Double](dimension)
          for (j <- 0 until numSents if y(j) == i) {
            for (k <- 0 until corpus.ngrams(j).length) {
              val textVector = corpus.vectors(corpus.ngrams(j)(k))
              newTheta += textVector * math.exp(qz(j)(k)(p))
            }
          }

          val sum = newTheta.sum
          theta(i)(p) =
            if (sum == 0 || sum.isNaN()) {
              //generateDirichlet(components, dimension, 1)(0)
              theta(i)(p)
            } else {
              newTheta / (newTheta.sum * MAGIC_DIVISOR)
            }

          //pw.println(theta(i)(p))
        }
      }

      //pw.close()

      /** compute Pi **/

      pw.println("PIs")
      for (i <- 0 until numClusters) {
        val newPi = DenseVector.zeros[Double](topicsPerCluster)
        for (p <- 0 until topicsPerCluster) {
          for (j <- 0 until numSents if y(j) == i) {
            for (k <- 0 until corpus.ngrams(j).length) {

              newPi(p) += math.exp(qz(j)(k)(p))
            }
          }
        }

        val sum = newPi.sum
        pi(i) =
          if (sum == 0 || sum.isNaN()) {
            //DenseVector.ones[Double](topicsPerCluster) / topicsPerCluster.asInstanceOf[Double]
            pi(i)
          } else {
            newPi / sum // multinomial distribution must sum up to one
          }

        //pw.println(i + ": " + pi(i))
      }
    }

    y.toArray
  }
}