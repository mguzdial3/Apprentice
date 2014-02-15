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
}