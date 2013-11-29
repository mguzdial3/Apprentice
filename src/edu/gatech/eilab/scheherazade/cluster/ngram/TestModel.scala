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
object TestModel {

  var ALPHA_SUM = 1500.0
  var DESIRED_DIMENSION = 1000
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

      //      for (i <- 0 until 100) {
      //        val vi = matrix(i, ::).toDenseVector
      //        val vj = matrix(i+1, ::).toDenseVector
      //        
      //        print(vi dot vi)
      //        print("\t")
      //        println(vi dot vj)
      //      }

      matrix
    }

  def performLargerPCA(oldCorpus: NGramCorpus): NGramCorpus =
    {

      val basis = loadPCABasis(DESIRED_DIMENSION)

      var newVectors = scala.collection.mutable.HashMap[String, DenseVector[Double]]()

      val iter = oldCorpus.vectors.iterator
      val matrix = DenseMatrix.zeros[Double](oldCorpus.vectors.size, DESIRED_DIMENSION)
      var row = 0
      for ((text, vector) <- iter) {
        val v = basis * vector
        //println("v=")
        //println(v)

        for (j <- 0 until DESIRED_DIMENSION) {
          matrix(row, j) = v(j)
        }
        row += 1
      }
      
      val pr = new PrintWriter(new BufferedOutputStream(new FileOutputStream("normal.txt")))
      
      for (i <- 0 until matrix.rows)
      {
        for(j <- 0 until matrix.cols)
        {
        	pr.print(matrix(i,j)+" ")        	
        }        
        pr.println
      }
//
      var list = List[(Int, Double)]()
      for (j <- 0 until DESIRED_DIMENSION) {
        val v = matrix(::, j)
        val max = v.max
        val min = v.min

        val p = (j, max - min)
        list = p :: list
      }
//
      list = list.sortBy(-_._2)
      println(list.mkString("\n"))
//      //histogram(matrix, 100)

      println("pca done")

      new NGramCorpus(oldCorpus.ngrams, newVectors.toMap, oldCorpus.stories)
    }

  def histogram(m: DenseMatrix[Double], col: Int) {
    val v = m(::, col)
    val min = v.min
    val max = v.max
    val bins = Array.ofDim[Int](1001)
    val size = (max - min) / 1000
    println("min = " + min)
    println("max = " + max)
    for (i <- 0 until v.size) {
      val b = math.floor((v(i) - min) / size).toInt
      bins(b) += 1
      println(v(i) + "->" + b)
    }

    for (b <- 0 until 1000) {
      if (bins(b) > 0) {
        print(b / size + min)
        print(":")
        println(bins(b))
      }
    }
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

  def testPca(data: DenseMatrix[Double], components: Int) = {
    val d = zeroMean(data)
    val (u, s, v) = svd(d)
    //val model = v(0 until components, ::) //top 'components' eigenvectors
    println("u size =" + u.rows + ", " + u.cols)
    println("s size =" + s.size)
    println("v size =" + v.rows + ", " + v.cols)
    //val filter = model.t * model
    //filter * d
    u(::, 0 until components) * diag(s(0 until components))
  }

  private def mean(v: Vector[Double]) = (v.valuesIterator.sum) / v.size

  private def zeroMean(m: DenseMatrix[Double]) = {
    val copy = m.copy
    for (c <- 0 until m.cols) {
      val col = copy(::, c)
      val colMean = mean(col)
      col -= colMean
    }
    // println("data \n" + m)
    // println("mean \n" + copy)
    copy
  }
  
  def performPCA(oldCorpus: NGramCorpus): NGramCorpus =
    {
      val desiredDimension = 400
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

      println("data size: " + matrix.rows + ", " + matrix.cols)
      
      val newData = testPca(matrix, desiredDimension)
      var newVectors = scala.collection.mutable.HashMap[String, DenseVector[Double]]()

      println("new data Size = " + newData.rows + ", " + newData.cols)
      var list = List[(Int, Double)]()
      for (j <- 0 until desiredDimension) {
        val v = newData(::, j)
        val max = v.max
        val min = v.min

        val p = (j, max - min)
        list = p :: list
      }

      list = list.sortBy(-_._2)
      println(list.mkString("\n"))

      println("pca done")

      new NGramCorpus(oldCorpus.ngrams, newVectors.toMap, oldCorpus.stories)
    }

  def normalizeLogProb(prob: Array[Double]): Array[Double] =
    {
      val sum = prob.sum
      val size = prob.size
      val mean = sum / size

      val newProb = Array.ofDim[Double](size)

      var divider = 0.0
      for (i <- 0 until size) {
        newProb(i) = prob(i) - mean
        divider = math.exp(newProb(i))
      }

      for (i <- 0 until size) {
        newProb(i) = newProb(i) - math.log(divider)
      }

      newProb
    }

  def train(oldCorpus: NGramCorpus): DenseVector[Int] = {
    println("s = " + ALPHA_SUM + " vector length = " + DESIRED_DIMENSION)
    val corpus = performLargerPCA(oldCorpus)

    DenseVector.zeros[Int](1000)
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