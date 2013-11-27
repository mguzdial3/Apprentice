package edu.gatech.eilab.scheherazade.cluster.ngram
import breeze.linalg.DenseVector

object ProbUtils {

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
  
    def normalizeLogProb(prob: DenseVector[Double]): DenseVector[Double] =
    {
      val sum = prob.sum
      val size = prob.size
      val mean = sum / size

      val newProb = DenseVector.zeros[Double](size)

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
}