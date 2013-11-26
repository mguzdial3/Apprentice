package edu.gatech.eilab.scheherazade.cluster.ngram

import breeze.stats.distributions._
import breeze.linalg._
import java.io._
import cc.mallet.optimize._
import scala.collection.mutable.ListBuffer
/**
 * a correct version of EM
 *
 */
object HMMModel2 {

  var ALPHA_SUM = 1500.0
  var DESIRED_DIMENSION = 150
  val SMALL = 1E-5

  val numClusters = 100

  val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("model.log")))

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

  def loadPCABasis(dimension: Int): DenseMatrix[Double] =
    {
      val matrix = DenseMatrix.zeros[Double](dimension, 1000)
      val lines = scala.io.Source.fromFile("pcabasis.txt").getLines

      for (i <- 0 until dimension) {
        val array = lines.next.split(", ")
        for (j <- 0 until 1000) {
          matrix(i, j) = array(j).trim.toDouble
        }
      }

      matrix
    }

  def performLargerPCA(oldCorpus: NGramCorpus): NGramCorpus =
    {

      val basis = loadPCABasis(DESIRED_DIMENSION)

      var newVectors = scala.collection.mutable.HashMap[String, DenseVector[Double]]()

      val iter = oldCorpus.vectors.iterator
      for ((text, vector) <- iter) {
        var newVector = basis * vector
        val min = newVector.min
        newVector = newVector - min + SMALL
        newVector = newVector / newVector.sum
        newVectors += ((text -> newVector))
      }

      println("pca done")

      new NGramCorpus(oldCorpus.ngrams, newVectors.toMap, oldCorpus.stories)
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

  def performPCA(oldCorpus: NGramCorpus): NGramCorpus =
    {
      val desiredDimension = 20
      val n = oldCorpus.vectors.size
      val m = oldCorpus.vectors.head._2.size
      val matrix = DenseMatrix.zeros[Double](n, m)

      var i = 0
      var iter = oldCorpus.vectors.iterator
      for ((text, vector) <- iter) {
        for (j <- 0 until m) {
          matrix.update(i, j, vector(j))
        }
        i += 1
      }

      val newData = PCA.pca(matrix, desiredDimension)
      var newVectors = scala.collection.mutable.HashMap[String, DenseVector[Double]]()

      i = 0
      iter = oldCorpus.vectors.iterator
      for ((text, _) <- iter) {
        var vector = newData(i, 0 until desiredDimension).toDenseVector
        val min = vector.min
        vector = vector - min + SMALL
        vector = vector / vector.sum
        newVectors += ((text -> vector))
        i += 1
      }

      println("pca done")

      new NGramCorpus(oldCorpus.ngrams, newVectors.toMap, oldCorpus.stories)
    }
  


  def train(oldCorpus: NGramCorpus): DenseVector[Int] = {
    println("s = " + ALPHA_SUM + " vector length = " + DESIRED_DIMENSION)
    val corpus = performLargerPCA(oldCorpus)
    val numSents = corpus.ngrams.length
    // number of unique ngrams
    val numNgrams = corpus.vectors.keys.size
    val DIMENSION = corpus.vectors.head._2.size
    val numStories = corpus.stories.size
    // manually set parameters

    val maxIterations = 40
    
    //val baseObservation = DenseVector.ones[Double](numClusters) * math.max(1.0, numSents / numClusters / 4)
    //var yPrior = Multinomial(baseObservation / baseObservation.sum)

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
    println("components: " + components.distinct)

    /** keep records if a cluster has attracted any sentences **/
    val drawnCrowd = Array.fill[Int](numClusters)(0)

    /** random initialization **/
    // Each sentence has a Y variable, which selects a cluster
    var y = DenseVector.rand(numSents, Rand.randInt(numClusters))

    // parameter for Dirichlet
    var theta = generateDirichlet(components, DIMENSION, numClusters).toArray

    var transition = Array.fill(numClusters, numClusters)(1.0 / numClusters)
    var portion = Array.fill(numClusters)(1.0 / numClusters)
    
    for (iter <- 1 to maxIterations) {

      println("iteration: " + iter)

      /** E-Step **/

      val oldY = y.copy

      for (s <- 0 until numStories) {

        // computing probability of each sentence belonging to each cluster
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
              val p = diri.logPdf(textVector) //+ yPrior.logProbabilityOf(j)
              product += p
            }

            prob(i)(j) = product + math.log(portion(j))
          }
          
          // normalization
          prob(i) = Utils.normalizeLogProb(prob(i))
        }

        //val time = System.currentTimeMillis()
        // run the ILP
        val ilp = new HMMAssignment(prob, Utils.logMat(transition))
        val assignment = ilp.solve
        
        //println("time used = " + (System.currentTimeMillis() - time) / 1000.0)

        for (i <- 0 until sentences.length) {
          val sID = sentences(i)
          y(sID) = assignment(i)
          //println(sID + ", " + assignment(i))
        }

      }

      if ((oldY - y).norm(1) < 0.01 || iter == maxIterations) {
        // converged or max iteration reached
        return y
      }

      val distinctIDs = (0 until numSents).map(y(_)).distinct
      val str = "non-empty clusters are: \n" + distinctIDs.mkString(", ") + " (" + distinctIDs.size + ")"
      pw.println(str)
      println(str)

      //      println("Stop? ")
      //      val answer = readLine.trim
      //      if (answer == "y" || answer == "Y") return y

      /** M-step **/
      /** compute theta **/

      (0 until numClusters).par.foreach { i =>
        var count = 0.0
        var dataList = ListBuffer[DenseVector[Double]]()
        for (j <- 0 until numSents if y(j) == i; k <- 0 until corpus.ngrams(j).size) {
          val textVector = corpus.vectors(corpus.ngrams(j)(k))
          dataList += textVector
        }

        //var newTheta: DenseVector[Double] = null

        if (dataList.size > 1) {
          var newTheta = optimize(dataList.toList, theta(i), components)
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
          /* resample the dirichlet distribution from the biggest cluster */

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

          var newTheta = DenseVector.zeros[Double](DIMENSION)
          for (k <- 0 until corpus.ngrams(randomSent).size) {
            newTheta += corpus.vectors(corpus.ngrams(randomSent)(k))
          }

          theta(i) = newTheta / (newTheta.sum / ALPHA_SUM)

        } else {
          drawnCrowd(i) -= 1
        }
      }

      /** compute transition probability **/

      transition = Array.fill(numClusters, numClusters)(0.0)
      for (s <- 0 until numStories) {
        val sentences = corpus.stories(s)
        for (i <- 0 until sentences.length - 1) {
          val currentCluster = y(sentences(i))
          val nextCluster = y(sentences(i + 1))
          transition(currentCluster)(nextCluster) += 1 // record all transitions
        }
      }

      // before normalization: makes sure nothing is zero
      for (i <- 0 until numClusters; j <- 0 until numClusters) {
        if (transition(i)(j) < SMALL) {
          transition(i)(j) = SMALL
        }
      }

      // perform normalization
      for (i <- 0 until numClusters) {
        val sum = transition(i).sum
        for (j <- 0 until numClusters) {
          transition(i)(j) = transition(i)(j) / sum 
        }
      }
      
      // compute the portion of each cluster
      portion = Array.fill(numClusters)(1.0)      
      for (i <- 0 until numSents)
      {
        portion(y(i)) += 1
      }
      
      for (i <- 0 until numClusters)
      {
        portion(i) += portion(i) / (numClusters + numSents)
      }
    }

    y
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

  def optimize(data: List[DenseVector[Double]], start: DenseVector[Double], components: List[Int]): DenseVector[Double] =
    {
      val n = start.size
      val func = new DiriOptimizable(data, start.toArray.clone, components.toArray)
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

  class DiriOptimizable(val data: List[DenseVector[Double]], start: Array[Double], val components: Array[Int]) extends Optimizable.ByGradientValue {

    def this(data: List[DenseVector[Double]], start: DenseVector[Double], components: Array[Int]) = this(data, start.toArray, components)

    val DELTA = 1e-5
    var x = start.clone
    val dimension = x.length
    val realDimension = components.length
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

    def getNumParameters() = realDimension

    def getParameter(index: Int) = x(components(index))

    def getParameters(buffer: Array[Double]) {
      for (i <- 0 until realDimension) {
        buffer(i) = x(components(i))
      }
    }

    def setParameter(index: Int, value: Double) {
      x(components(index)) = value
    }

    def setParameters(params: Array[Double]) {
      for (i <- 0 until realDimension) {
        x(components(i)) = params(i)
      }
    }

    def getValue(): Double =
      {
        //math.exp(data.map(Dirichlet(x).logPdf).sum - subtractee)
        data.map(Dirichlet(x).logPdf).sum //- subtractee
      }

    def getValueGradient(buffer: Array[Double]) {
      for (i <- 0 until realDimension) {
        val index = components(i)
        var x1 = DenseVector(x.clone)
        x1(index) = x1(index) + DELTA

        var x2 = DenseVector(x.clone)
        x2(index) = x2(index) - DELTA

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