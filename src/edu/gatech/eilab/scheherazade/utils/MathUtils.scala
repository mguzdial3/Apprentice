package edu.gatech.eilab.scheherazade.utils

object MathUtils {

  def factorial(n: Int) =
    {
      var product = 1.0
      for (i <- 1 to n) {
        product = product * i
      }
      product
    }

  def logFactorial(n: Int): Double =
    {
      var result = 0.0
      for (i <- 2 to n) result += math.log(i)

      result
    }
}