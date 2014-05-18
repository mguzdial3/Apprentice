package edu.gatech.eilab.scheherazade.cluster.ngram
import breeze.linalg.DenseVector
import breeze.linalg._

object Utils {
  val SMALL = 1E-5

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
        var vector = newData(i, 0 until desiredDimension).t //toDenseVector
        val min = vector.min
        vector = vector - min + SMALL
        vector = vector / vector.sum
        newVectors += ((text -> vector))
        i += 1
      }

      println("pca done")

      new NGramCorpus(oldCorpus.ngrams, newVectors.toMap, oldCorpus.stories)
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

  def performLargerPCA(oldCorpus: NGramCorpus, desiredDimension: Int): NGramCorpus =
    {

      val basis = loadPCABasis(desiredDimension)

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
   * Input = a discrete probability distribution in log space, which has not been normalized
   *  Output = a discrete probability distribution after normalization, but still in log space
   */
  def normalizeLogProb(prob: Array[Double]): Array[Double] =
    {
      //val sum = prob.sum
      val size = prob.size
      val max = prob.max

      val newProb = Array.ofDim[Double](size)

      var divider = 0.0
      for (i <- 0 until size) {
        newProb(i) = math.exp(prob(i) - max + 2) + 1e-6
        divider += newProb(i)
      }

      val substractee = math.log(divider)

      for (i <- 0 until size) {
        newProb(i) = math.log(newProb(i)) - substractee
      }

      newProb
    }

  def normalizeLogProb(prob: DenseVector[Double]): DenseVector[Double] =
    {
      val sum = prob.sum
      val size = prob.size
      val max = prob.max

      val newProb = DenseVector.zeros[Double](size)

      var divider = 0.0
      for (i <- 0 until size) {
        newProb(i) = math.exp(prob(i) - max + 2) + 1e-6
        divider += newProb(i)
      }

      for (i <- 0 until size) {
        newProb(i) = math.log(newProb(i)) - math.log(divider)
      }

      newProb
    }

  /**
   * take log of each term in a matrix
   *
   */
  def logMat(matrix: Array[Array[Double]]): Array[Array[Double]] =
    {
      val n = matrix.length
      val m = matrix(0).length
      val result = Array.ofDim[Double](n, m)

      for (i <- 0 until n; j <- 0 until m) {
        result(i)(j) = math.log(matrix(i)(j))
      }

      result
    }
}