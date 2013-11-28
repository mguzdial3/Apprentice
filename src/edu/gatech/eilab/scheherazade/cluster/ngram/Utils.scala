package edu.gatech.eilab.scheherazade.cluster.ngram
import breeze.linalg.DenseVector

object Utils {

  /**
   * Input = a discrete probability distribution in log space, which has not been normalized
   *  Output = a discrete probability distribution after normalization, but still in log space
   */
  def normalizeLogProb(prob: Array[Double]): Array[Double] =
    {
      val sum = prob.sum
      val size = prob.size
      val mean = sum / size

      val newProb = Array.ofDim[Double](size)

      var divider = 0.0
      for (i <- 0 until size) {
        newProb(i) = prob(i) - mean
        divider += math.exp(newProb(i))
      }

      val substractee = math.log(divider)

      for (i <- 0 until size) {
        newProb(i) = newProb(i) - substractee
      }

      newProb
    }

  def normalizeLogProb(prob: DenseVector[Double]): DenseVector[Double] =
    {
      val sum = prob.sum
      val size = prob.size
      val mean = sum / size

      val newProb = DenseVector.zeros[Double](size)

      var divider = 0.0
      for (i <- 0 until size) {
        newProb(i) = prob(i) - mean
        divider += math.exp(newProb(i))
      }

      for (i <- 0 until size) {
        newProb(i) = newProb(i) - math.log(divider)
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