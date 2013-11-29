package edu.gatech.eilab.scheherazade.cluster.ngram

object Utils {
  
  val SMALL = 1E-7
  
  def normalizeLogProb(prob: Array[Double]): Array[Double] =
    {
    
      //println("normal")
      //val sum = prob.sum
      val size = prob.size
      val max = prob.max

      val newProb = Array.ofDim[Double](size)

      var divider = 0.0
      for (i <- 0 until size) {
        newProb(i) = math.exp(prob(i) - max + 1) + SMALL
        divider += newProb(i)
      }

      if (divider < 1e-10 || newProb.contains(Double.NegativeInfinity) || newProb.contains(Double.PositiveInfinity)) {
        println("log prob = ")
        for (i <- 0 until size) {
          println(prob(i))
        }
        println("max = " + max)
        println("new prob = ")
        for (i <- 0 until size) {
          println(prob(i))
        }
        println("divider = " + divider)
      }

      val logDivider = math.log(divider)
      for (i <- 0 until size) {
        newProb(i) = math.log(newProb(i)) - logDivider
      }

      newProb
    }
}