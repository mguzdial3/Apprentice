package edu.gatech.eilab.scheherazade.cluster.ngram

import breeze.stats.distributions._
import breeze.linalg._
import java.io._
import cc.mallet.optimize._
import scala.collection.mutable.ListBuffer
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.cluster.metric._

/**
 * with garbage cluster
 *
 */
object ILPModel4 {

  var ALPHA_SUM = 1500.0
  var DESIRED_DIMENSION = 150
  val SMALL = 1E-5

  val numClusters = 130

  //val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("model.log")))

  def saveProb(prob: Array[Array[Double]]) {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("prob.log")))

    for (i <- 0 until prob.length) {
      for (j <- 0 until prob(0).length) {
        pw.print(prob(i)(j))
        pw.print("\t")
      }
      pw.println
    }
    pw.close
  }

  def mean(diri: Dirichlet[DenseVector[Double], Int]): DenseVector[Double] =
    {
      val params = diri.params
      val sum = params.sum
      val size = params.size
      val array = Array.ofDim[Double](size)
      for (i <- 0 until size) {
        array(i) = params(i) / sum
      }

      new DenseVector(array)
    }

  /**
   * generates n DenseVectors which will be used as parameters for Dirichlet distributions
   *
   */
  def generateDirichlet(components: List[Int], dimension: Int, n: Int) =
    {

      val total = 1 * ALPHA_SUM - (dimension - components.size) * SMALL

      val base = DenseVector.zeros[Double](dimension)
      val range = (0 until dimension).filterNot(components contains)
      for (i <- range) {
        base(i) = SMALL
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

  /**
   * generates n DenseVectors which will be used as parameters for Dirichlet distributions
   *
   */
  def generateFullDirichlet(dimension: Int, n: Int) =
    {

      val total = 1 * ALPHA_SUM - dimension * SMALL

      val base = DenseVector.zeros[Double](dimension)
      for (i <- 0 until dimension) {
        base(i) = SMALL
      }

      for (i <- 1 to n) yield {
        val vector = DenseVector.zeros[Double](dimension)
        var sum = 0.0
        for (i <- 0 until dimension) {
          val v = Rand.uniform.draw
          vector(i) = v
          sum += v
        }

        vector / sum * total + base
      }
    }

  def train(oldCorpus: NGramCorpus, sents: List[Sentence], gold: List[Cluster]): DenseVector[Int] = {
    println("s = " + ALPHA_SUM + " vector length = " + DESIRED_DIMENSION)
    val corpus = Utils.performLargerPCA(oldCorpus, DESIRED_DIMENSION)
    val numSents = corpus.ngrams.length
    // number of unique ngrams
    val numNgrams = corpus.vectors.keys.size
    val DIMENSION = corpus.vectors.head._2.size
    val numStories = corpus.stories.size
    // manually set parameters

    val maxIterations = 30
    val GARBAGE_RATIO = 0.2
    val GARBAGE_PROB = 0.01
    val GARBAGE_LOG_PROB = 1500
    //val baseObservation = DenseVector.ones[Double](numClusters) * math.max(1.0, numSents / numClusters / 4)
    //var yPrior = Multinomial(baseObservation / baseObservation.sum)

    /** keep records if a cluster has attracted any sentences **/
    val drawnCrowd = Array.fill[Int](numClusters)(0)

    /** random initialization **/
    // Each sentence has a Y variable, which selects a cluster
    var y = DenseVector.rand(numSents, Rand.randInt(numClusters))

    // parameter for Dirichlet
    var theta = generateFullDirichlet(DIMENSION, numClusters).toArray
    //theta(numClusters - 1) = optimize(corpus.vectors.values.toList, theta(numClusters - 1))
    
    // prior for each cluster
    var portion = Array.fill[Double](numClusters - 1)(1.0 / (numClusters - 1))

    for (iter <- 1 to maxIterations) {

      println("iteration: " + iter)
      if (iter == 3)
      {
        theta(numClusters - 1) = DenseVector.fill(DIMENSION)(1)
      }
      /** E-Step **/

      val oldY = y.copy
      // computing y = argmax P(x,z|y)
      for (s <- 0 until numStories) {

        val sentences = corpus.stories(s)
        val prob = Array.ofDim[Double](sentences.length, numClusters) // probabilities of each sentence belonging to each cluster

        for (i <- 0 until sentences.length) {
          // outer loop for each sentence
          val sID = sentences(i)

          for (j <- 0 until numClusters) {
            // loop for each cluster
            var product = 0.0
            val diri = new Dirichlet(theta(j))

            for (text <- corpus.ngrams(sID)) {
              // for each ngram in the sentence              
              val textVector = corpus.vectors(text)
              val p = diri.logPdf(textVector)
              product += p
            }

            if (j < numClusters - 1) {
              prob(i)(j) = product + math.log(portion(j)) + math.log(1 - GARBAGE_RATIO)
            } else {
            	prob(i)(j) = product + math.log(GARBAGE_RATIO)
            }
          }

          // garbage cluster
          //          if (iter > 3)
          //prob(i)(numClusters - 1) = GARBAGE_LOG_PROB
          //          else
          //            prob(i)(numClusters - 1) = -100000

          //          if (iter > 3) {
          //
          //println(i + "===" + prob(i).mkString(" "))
          //          }

          prob(i) = Utils.normalizeLogProb(prob(i))
        }

        // run the ILP
        //        if (iter > 3)
        //          saveProb(prob)

        val ilp = new ILPAssignmentGarbage(prob)
        val assignment = ilp.solve
        print(".")

        for (i <- 0 until sentences.length) {
          val sID = sentences(i)
          y(sID) = assignment(i)
          //println(sID + ", " + assignment(i))
        }

      }

      if (((oldY - y).norm(1) < 0.01 && iter >= maxIterations * 0.8) || iter == maxIterations) {
        // converged or max iteration reached
        return y
      }

      val distinctIDs = (0 until numSents).map(y(_)).distinct
      val str = "non-empty clusters are: " + " (" + distinctIDs.size + ")"
      //pw.println(str)
      println(str)
      println("garbage = " + y.toArray.count(_ == numClusters - 1))

      /** evaluation **/
      val foundClusters = NGramizer.peelClusters(y, sents)
      val noGarbage = foundClusters.filterNot(_.members.size < 4)
      val (p1, r1, p2, r2, purity) = ClusterMetric.evaluate(noGarbage, gold.filterNot(_.members.size < 4))

      //      println("Stop? ")
      //      val answer = readLine.trim
      //      if (answer == "y" || answer == "Y") return y

      /** M-step **/
      /** compute theta **/

      (0 until numClusters - 1).par.foreach { i =>
        var count = 0.0
        var dataList = ListBuffer[DenseVector[Double]]()
        for (j <- 0 until numSents if y(j) == i; k <- 0 until corpus.ngrams(j).size) {
          val textVector = corpus.vectors(corpus.ngrams(j)(k))
          dataList += textVector
        }

        //var newTheta: DenseVector[Double] = null

        if (dataList.size > 1) {
          var newTheta = optimize(dataList.toList, theta(i))
          theta(i) = newTheta

          drawnCrowd(i) = 3

          //pw.println("new params: " + theta(i))
        } else if (dataList.size == 1) {

          var newTheta = DenseVector.zeros[Double](DIMENSION)
          for (item <- dataList) {
            newTheta += item
          }

          theta(i) = newTheta / (newTheta.sum / ALPHA_SUM)

          drawnCrowd(i) = 3
        } else if (drawnCrowd(i) <= 0) {

          // a random cluster id with probability proportional to cluster size
          val roulette = Multinomial(DenseVector(computePortion(y, numClusters)))
          val randomCluster = roulette.draw

          // draw a random sentence if the cluster contains two or more sentences
          val sentsInCluster = (0 until numSents).filter(y(_) == randomCluster)
          if (sentsInCluster.size > 1) {
            val idx = Rand.randInt(sentsInCluster.size).draw
            val randomSent = sentsInCluster(idx)

            var newTheta = DenseVector.zeros[Double](DIMENSION)
            for (k <- 0 until corpus.ngrams(randomSent).size) {
              newTheta += corpus.vectors(corpus.ngrams(randomSent)(k))
            }

            theta(i) = newTheta / (newTheta.sum / ALPHA_SUM)
          }
        } else {
          drawnCrowd(i) -= 1
        }
      }

      /**
       * re-compute the multinomial prior for y **
       */
      portion = computePortion(y, numClusters)

    }

    y
  }

  def computePortion(y: DenseVector[Int], numClusters: Int): Array[Double] =
    {
      val numSents = y.size
      val portion = Array.fill[Double](numClusters - 1)(0.5) // Jeffery's prior
      var ps = 0
      for (j <- 0 until numSents) {
        if (y(j) < numClusters - 1) {
          portion(y(j)) += 1
          ps += 1
        }
      }

      portion.map(_ / ps)
    }

  def randomSentFromBiggestCluster(numSents: Int, y: DenseVector[Int]) = {
    var max = 0
    var maxIdx = -1
    for (q <- 0 until numClusters) {
      var count = 0
      y.foreach { x => if (x == q) count += 1 }
      if (count > max) {
        max = count
        maxIdx = q
      }
    }

    // a random sentence id from the biggest cluster
    val randomSent = {
      val sentsInCluster = (0 until numSents).filter(y(_) == maxIdx)
      val idx = Rand.randInt(sentsInCluster.size).draw
      sentsInCluster(idx)
    }

    randomSent
  }

  def optimize(data: List[DenseVector[Double]], start: DenseVector[Double]): DenseVector[Double] =
    {
      val n = start.size
      val func = new DiriOptimizable(data, start.toArray.clone)
      //println("old value = " + func.getValue)
      try {
        val optimizer = new LimitedMemoryBFGS(func)
        System.setErr(new PrintStream(new OutputStream() {
          def write(b: Int) {
          }
        }))

        //optimizer.setTolerance(1e-9)
        optimizer.optimize(20)
      } catch {
        case x: OptimizationException => //println(x.getMessage())
      }

      val result = func.getFullParameters
      //func.getParameters(result)
      //println("new value = " + func.getValue)
      new DenseVector(result)
    }

  class DiriOptimizable(val data: List[DenseVector[Double]], start: Array[Double]) extends Optimizable.ByGradientValue {

    def this(data: List[DenseVector[Double]], start: DenseVector[Double]) = this(data, start.toArray)

    val DELTA = 1e-5
    var x = start.clone
    val dimension = x.length
    val dataSize = data.size
    //  val subtractee = {
    //    val d = Dirichlet(x)
    //    var sum = 0.0
    //    for (item <- data) {
    //      sum += d.logPdf(item)
    //    }
    //    sum
    //  }

    def getFullParameters() = x

    def getNumParameters() = dimension

    def getParameter(index: Int) = x(index)

    def getParameters(buffer: Array[Double]) {
      for (i <- 0 until dimension) {
        buffer(i) = x(i)
      }
    }

    def setParameter(index: Int, value: Double) {
      x(index) = value
    }

    def setParameters(params: Array[Double]) {
      for (i <- 0 until dimension) {
        x(i) = params(i)
      }
    }

    def getValue(): Double =
      {
        //math.exp(data.map(Dirichlet(x).logPdf).sum - subtractee)
        data.map(Dirichlet(x).logPdf).sum //- subtractee
      }

    def getValueGradient(buffer: Array[Double]) {
      for (i <- 0 until dimension) {
        var x1 = DenseVector(x.clone)
        x1(i) = x1(i) + DELTA

        var x2 = DenseVector(x.clone)
        x2(i) = x2(i) - DELTA

        //      val prob1 = math.exp(data.map(Dirichlet(x1).logPdf).sum - subtractee)
        //      val prob2 = math.exp(data.map(Dirichlet(x2).logPdf).sum - subtractee)

        val prob1 = data.map(Dirichlet(x1).logPdf).sum //- subtractee
        val prob2 = data.map(Dirichlet(x2).logPdf).sum //- subtractee

        buffer(i) = (prob1 - prob2) / DELTA / 2
        //println(i + ": " + buffer(i))
      }
    }
  }
}